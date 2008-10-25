
MSMProp2, a simplified but functionally equivalent version of MSMProp1
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Julian Seward, OpenWorks Ltd, 19 August 2008
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that this file does NOT describe the state machine used in the
svn://svn.valgrind.org/branches/YARD version of Helgrind.  That state
machine is different again from any previously described machine.

See the file README_YARD.txt for more details on YARD.

                     ----------------------

In early 2008 Konstantin Serebryany proposed "MSMProp1", a memory
state machine for data race detection.  It is described at
http://code.google.com/p/data-race-test/wiki/MSMProp1

Implementation experiences show MSMProp1 is useful, but difficult to
implement efficiently.  In particular keeping the memory usage under
control is complex and difficult.

This note points out a key simplification of MSMProp1, which makes it
easier to implement without changing the functionality.


The idea
~~~~~~~~

The core of the idea pertains to the "Condition" entry for MSMProp1
state machine rules E5 and E6(r).  These are, respectively:

    HB(SS, currS)  and its negation
    ! HB(SS, currS).

Here, SS is a set of segments, and currS is a single segment.  Each
segment contains a vector timestamp.  The expression "HB(SS, currS)"
is intended to denote

   for each segment S in SS  .  happens_before(S,currS)

where happens_before(S,T) means that S's vector timestamp is ordered
before-or-equal to T's vector timestamp.

In words, the expression

   for each segment S in SS  .  happens_before(S,currS)

is equivalent to saying that currS has a timestamp which is
greater-than-equal to the timestamps of all the segments in SS.

The key observation is that this is equivalent to

   happens_before( JOIN(SS), currS )

where JOIN is the lattice-theoretic "max" or "least upper bound"
operation on vector clocks.  Given the definition of HB,
happens_before and (binary) JOIN, this is easy to prove.


The consequences
~~~~~~~~~~~~~~~~

With that observation in place, it is a short step to observe that
storing segment sets in MSMProp1 is unnecessary.  Instead of
storing a segment set in each shadow value, just store and
update a single vector timestamp.  The following two equivalences
hold:

   MSMProp1                        MSMProp2

   adding a segment S              join-ing S's vector timestamp
   to the segment-set              to the current vector timestamp

   HB(SS,currS)                    happens_before(
                                      currS's timestamp,
                                      current vector timestamp )

Once it is no longer necessary to represent segment sets, it then
also becomes unnecessary to represent segments.  This constitutes
a significant simplication to the implementation.


The resulting state machine, MSMProp2
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

MSMProp2 is isomorphic to MSMProp1, with the following changes:

States are    New,   Read(VTS,LS),   Write(VTS,LS)

where LS is a lockset (as before) and VTS is a vector timestamp.

For a thread T with current lockset 'currLS' and current VTS 'currVTS'
making a memory access, the new rules are

Name  Old-State         Op  Guard         New-State              Race-If

E1  New                 rd  True          Read(currVTS,currLS)   False

E2  New                 wr  True          Write(currVTS,currLS)  False

E3  Read(oldVTS,oldLS)  rd  True          Read(newVTS,newLS)     False

E4  Read(oldVTS,oldLS)  wr  True          Write(newVTS,newLS)    #newLS == 0 
                                                                 && !hb(oldVTS,currVTS)

E5  Write(oldVTS,oldLS) rd  hb(oldVTS,    Read(currVTS,currLS)   False
                               currVTS)

E6r Write(oldVTS,oldLS) rd  !hb(oldVTS,   Write(newVTS,newLS)    #newLS == 0 
                                currVTS)                         && !hb(oldVTS,currVTS)

E6w Write(oldVTS,oldLS) wr  True          Write(newVTS,newLS)    #newLS == 0 
                                                                 && !hb(oldVTS,currVTS)

   where newVTS = join2(oldVTS,currVTS)

         newLS  = if   hb(oldVTS,currVTS)
                  then currLS
                  else intersect(oldLS,currLS)

         hb(vts1, vts2) =  vts1 happens before or is equal to vts2


Interpretation of the states
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

I always found the state names in MSMProp1 confusing.  Both MSMProp1
and MSMProp2 are easier to understand if the states Read and Write are
renamed, like this:

   old name           new name

   Read               WriteConstraint
   Write              AllConstraint

The effect of a state Read(VTS,LS) is to constrain all later-observed
writes so that either (1) the writing thread holds at least one lock
in common with LS, or (2) those writes must happen-after VTS.  If
neither of those two conditions hold, a race is reported.

Hence a Read state places a constraint on writes.

The effect of a state Write(VTS,LS) is similar, but it applies to all
later-observed accesses: either (1) the accessing thread holds at
least one lock in common with LS, or (2) those accesses must
happen-after VTS.  If neither of those two conditions hold, a race is
reported.

Hence a Write state places a constraint on all accesses.

If we ignore the LS component of these states, the intuitive
interpretation of the VTS component is that it states the earliest
vector-time that the next write / access may safely happen.

