
//--------------------------------------------------------------------*/
//--- DHAT: a Dynamic Heap Analysis Tool                dh_test.js ---*/
//--------------------------------------------------------------------*/

/*
   This file is part of DHAT, a Valgrind tool for profiling the
   heap usage of programs.

   Copyright (C) 2018 Mozilla Foundation

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307, USA.

   The GNU General Public License is contained in the file COPYING.
*/

// We can't fully automate testing of a web app via the normal Valgrind
// regression testing. Instead we have this code, which is executed when
// dh_view.html is loaded with a "?test=1" parameter.
//
// Things tested by this file:
// - Tree building, with multiple sort metrics.
// - Text content of the displayed tree.
//
// Things not tested by this file:
// - Output from DHAT itself (unless that output is regenerated when necessary
//   and copy-and-pasted in the "input" fields in this file).
// - Interactions with the "Load" button and "Sort metric" menu.
// - File loading and parsing.
// - Non-text content of the displayed tree (e.g. node colours, sortKey
//   highlighting).
// - Tree interactions (collapsing and expanding of nodes).

"use strict";

// Test inputs are copied verbatim from DHAT output files, not as strings but
// as actual JavaScript code. This works because output files are JSON, and
// JSON is valid JavaScript.
//
// Expected outputs are paired with a sort metric, and copied verbatim from the
// DHAT viewer.
let tests = []

//---------------------------------------------------------------------------
// empty (corresponds to dhat/tests/empty.c)
//---------------------------------------------------------------------------

let empty = {
  name: "empty",
  input:
//---------------------------------------------------------------------------
{"dhatFileVersion":1
,"cmd":"./empty"
,"pid":23431
,"mi":0,"ei":248602
,"aps":
 [
 ]
,"ftbl":
 ["[root]"
 ]
}
//---------------------------------------------------------------------------
  ,
  outputs: [
    {
      label: "Total (bytes)",
      expected:
//---------------------------------------------------------------------------
`\
Invocation {
  Command: ./empty
  PID:     23431
}

Times {
  t-gmax: 0 instrs (0% of program duration)
  t-end:  248,602 instrs
}

─ AP 1/1 {
    Total:     0 bytes (0%, 0/Minstr) in 0 blocks (0%, 0/Minstr), avg size 0 bytes, avg lifetime 0 instrs (0% of program duration)
    At t-gmax: 0 bytes (0%) in 0 blocks (0%), avg size 0 bytes
    At t-end:  0 bytes (0%) in 0 blocks (0%), avg size 0 bytes
    Reads:     0 bytes (0%, 0/Minstr), 0/byte
    Writes:    0 bytes (0%, 0/Minstr), 0/byte
    Allocated at {
      #0: [root]
    }
  }

AP significance threshold: total >= 0 bytes (0%)
`
//---------------------------------------------------------------------------
    }
  ]
};
tests.push(empty);

//---------------------------------------------------------------------------
// single (corresponds to dhat/tests/single.c)
//---------------------------------------------------------------------------

let single = {
  name: "single",
  input:
//---------------------------------------------------------------------------
{"dhatFileVersion":1
,"cmd":"./single"
,"pid":30563
,"mi":242900,"ei":249824
,"aps":
 [{"tb":16,"tbk":1,"tli":6924
  ,"mb":16,"mbk":1
  ,"gb":16,"gbk":1
  ,"fb":16,"fbk":1
  ,"rb":0,"wb":12
  ,"acc":[-4,3,-12,0]
  ,"fs":[1]
  }
 ]
,"ftbl":
 ["[root]"
 ,"0x10865B: main (single.cpp:4)"
 ]
}
//---------------------------------------------------------------------------
  ,
  outputs: [
    {
      label: "Total (bytes)",
      expected:
//---------------------------------------------------------------------------
`\
Invocation {
  Command: ./single
  PID:     30563
}

Times {
  t-gmax: 242,900 instrs (97.23% of program duration)
  t-end:  249,824 instrs
}

─ AP 1/1 {
    Total:     16 bytes (100%, 64.05/Minstr) in 1 blocks (100%, 4/Minstr), avg size 16 bytes, avg lifetime 6,924 instrs (2.77% of program duration)
    At t-gmax: 16 bytes (100%) in 1 blocks (100%), avg size 16 bytes
    At t-end:  16 bytes (100%) in 1 blocks (100%), avg size 16 bytes
    Reads:     0 bytes (0%, 0/Minstr), 0/byte
    Writes:    12 bytes (100%, 48.03/Minstr), 0.75/byte
    Accesses: {
      [  0]  3 〃 〃 〃 - - - - - - - - - - - - 
    }
    Allocated at {
      #0: [root]
      #1: 0x10865B: main (single.cpp:4)
    }
  }

AP significance threshold: total >= 0.16 bytes (1%)
`
//---------------------------------------------------------------------------
    }
  ]
};
tests.push(single);

//---------------------------------------------------------------------------
// subseqs (a synthetic test for locations that are subsequences of other
// locations, which are rare but can happen in practice, esp. with recursion)
//---------------------------------------------------------------------------

let subseqs = {
  name: "subseqs",
  input:
//---------------------------------------------------------------------------
{"dhatFileVersion":1
,"cmd":"subseqs"
,"pid":0
,"mi":10000,"ei":20000
,"aps":
 [{"tb":15,"tbk":1,"tli":1000
  ,"mb":15,"mbk":1
  ,"gb":15,"gbk":1
  ,"fb":0,"fbk":0
  ,"rb":0,"wb":0
  ,"acc":[-15,0]
  ,"fs":[1,2,3]
  }
 ,{"tb":14,"tbk":1,"tli":1000
  ,"mb":14,"mbk":1
  ,"gb":14,"gbk":1
  ,"fb":0,"fbk":0
  ,"rb":0,"wb":0
  ,"acc":[-14,0]
  ,"fs":[1,2,3,3]
  }
 ,{"tb":13,"tbk":1,"tli":1000
  ,"mb":13,"mbk":1
  ,"gb":13,"gbk":1
  ,"fb":0,"fbk":0
  ,"rb":0,"wb":0
  ,"acc":[-13,0]
  ,"fs":[1,2,3,3,3]
  }
 ,{"tb":12,"tbk":1,"tli":1000
  ,"mb":12,"mbk":1
  ,"gb":12,"gbk":1
  ,"fb":0,"fbk":0
  ,"rb":0,"wb":0
  ,"acc":[-12,0]
  ,"fs":[4,5,6,6,6]
  }
 ,{"tb":11,"tbk":1,"tli":1000
  ,"mb":11,"mbk":1
  ,"gb":11,"gbk":1
  ,"fb":0,"fbk":0
  ,"rb":0,"wb":0
  ,"acc":[-11,0]
  ,"fs":[4,5,6,6]
  }
 ,{"tb":10,"tbk":1,"tli":1000
  ,"mb":10,"mbk":1
  ,"gb":10,"gbk":1
  ,"fb":0,"fbk":0
  ,"rb":0,"wb":0
  ,"acc":[-10,0]
  ,"fs":[4,5,6]
  }
 ,{"tb":9,"tbk":1,"tli":1000
  ,"mb":9,"mbk":1
  ,"gb":9,"gbk":1
  ,"fb":0,"fbk":0
  ,"rb":0,"wb":0
  ,"acc":[-9,0]
  ,"fs":[7,8,9]
  }
 ,{"tb":8,"tbk":1,"tli":1000
  ,"mb":8,"mbk":1
  ,"gb":8,"gbk":1
  ,"fb":0,"fbk":0
  ,"rb":0,"wb":0
  ,"acc":[-8,0]
  ,"fs":[7,8,10]
  }
 ,{"tb":7,"tbk":1,"tli":1000
  ,"mb":7,"mbk":1
  ,"gb":7,"gbk":1
  ,"fb":0,"fbk":0
  ,"rb":0,"wb":0
  ,"acc":[-7,0]
  ,"fs":[7,8]
  }
 ]
,"ftbl":
 ["[root]"
 ,"a()"
 ,"b()"
 ,"c()"
 ,"d()"
 ,"e()"
 ,"f()"
 ,"g()"
 ,"h()"
 ,"i()"
 ,"j()"
 ]
}
//---------------------------------------------------------------------------
  ,
  outputs: [
    {
      label: "Total (bytes)",
      expected:
//---------------------------------------------------------------------------
`\
Invocation {
  Command: subseqs
  PID:     0
}

Times {
  t-gmax: 10,000 instrs (50% of program duration)
  t-end:  20,000 instrs
}

▼ AP 1/1 (3 children) {
    Total:     99 bytes (100%, 4,950/Minstr) in 9 blocks (100%, 450/Minstr), avg size 11 bytes, avg lifetime 1,000 instrs (5% of program duration)
    At t-gmax: 99 bytes (100%) in 9 blocks (100%), avg size 11 bytes
    At t-end:  0 bytes (0%) in 0 blocks (0%), avg size 0 bytes
    Reads:     0 bytes (0%, 0/Minstr), 0/byte
    Writes:    0 bytes (0%, 0/Minstr), 0/byte
    Allocated at {
      #0: [root]
    }
  }
  ├─▼ AP 1.1/3 (2 children) {
  │     Total:     42 bytes (42.42%, 2,100/Minstr) in 3 blocks (33.33%, 150/Minstr), avg size 14 bytes, avg lifetime 1,000 instrs (5% of program duration)
  │     At t-gmax: 42 bytes (42.42%) in 3 blocks (33.33%), avg size 14 bytes
  │     At t-end:  0 bytes (0%) in 0 blocks (0%), avg size 0 bytes
  │     Reads:     0 bytes (0%, 0/Minstr), 0/byte
  │     Writes:    0 bytes (0%, 0/Minstr), 0/byte
  │     Allocated at {
  │       #1: a()
  │       #2: b()
  │       #3: c()
  │     }
  │   }
  │   ├─▼ AP 1.1.1/2 (2 children) {
  │   │     Total:     27 bytes (27.27%, 1,350/Minstr) in 2 blocks (22.22%, 100/Minstr), avg size 13.5 bytes, avg lifetime 1,000 instrs (5% of program duration)
  │   │     At t-gmax: 27 bytes (27.27%) in 2 blocks (22.22%), avg size 13.5 bytes
  │   │     At t-end:  0 bytes (0%) in 0 blocks (0%), avg size 0 bytes
  │   │     Reads:     0 bytes (0%, 0/Minstr), 0/byte
  │   │     Writes:    0 bytes (0%, 0/Minstr), 0/byte
  │   │     Allocated at {
  │   │       ^1: a()
  │   │       ^2: b()
  │   │       ^3: c()
  │   │       #4: c()
  │   │     }
  │   │   }
  │   │   ├── AP 1.1.1.1/2 {
  │   │   │     Total:     14 bytes (14.14%, 700/Minstr) in 1 blocks (11.11%, 50/Minstr), avg size 14 bytes, avg lifetime 1,000 instrs (5% of program duration)
  │   │   │     Max:       14 bytes in 1 blocks, avg size 14 bytes
  │   │   │     At t-gmax: 14 bytes (14.14%) in 1 blocks (11.11%), avg size 14 bytes
  │   │   │     At t-end:  0 bytes (0%) in 0 blocks (0%), avg size 0 bytes
  │   │   │     Reads:     0 bytes (0%, 0/Minstr), 0/byte
  │   │   │     Writes:    0 bytes (0%, 0/Minstr), 0/byte
  │   │   │     Accesses: {
  │   │   │       [  0]  - - - - - - - - - - - - - - 
  │   │   │     }
  │   │   │     Allocated at {
  │   │   │       ^1: a()
  │   │   │       ^2: b()
  │   │   │       ^3: c()
  │   │   │       ^4: c()
  │   │   │     }
  │   │   │   }
  │   │   └── AP 1.1.1.2/2 {
  │   │         Total:     13 bytes (13.13%, 650/Minstr) in 1 blocks (11.11%, 50/Minstr), avg size 13 bytes, avg lifetime 1,000 instrs (5% of program duration)
  │   │         Max:       13 bytes in 1 blocks, avg size 13 bytes
  │   │         At t-gmax: 13 bytes (13.13%) in 1 blocks (11.11%), avg size 13 bytes
  │   │         At t-end:  0 bytes (0%) in 0 blocks (0%), avg size 0 bytes
  │   │         Reads:     0 bytes (0%, 0/Minstr), 0/byte
  │   │         Writes:    0 bytes (0%, 0/Minstr), 0/byte
  │   │         Accesses: {
  │   │           [  0]  - - - - - - - - - - - - - 
  │   │         }
  │   │         Allocated at {
  │   │           ^1: a()
  │   │           ^2: b()
  │   │           ^3: c()
  │   │           ^4: c()
  │   │           #5: c()
  │   │         }
  │   │       }
  │   └── AP 1.1.2/2 {
  │         Total:     15 bytes (15.15%, 750/Minstr) in 1 blocks (11.11%, 50/Minstr), avg size 15 bytes, avg lifetime 1,000 instrs (5% of program duration)
  │         Max:       15 bytes in 1 blocks, avg size 15 bytes
  │         At t-gmax: 15 bytes (15.15%) in 1 blocks (11.11%), avg size 15 bytes
  │         At t-end:  0 bytes (0%) in 0 blocks (0%), avg size 0 bytes
  │         Reads:     0 bytes (0%, 0/Minstr), 0/byte
  │         Writes:    0 bytes (0%, 0/Minstr), 0/byte
  │         Accesses: {
  │           [  0]  - - - - - - - - - - - - - - - 
  │         }
  │         Allocated at {
  │           ^1: a()
  │           ^2: b()
  │           ^3: c()
  │         }
  │       }
  ├─▼ AP 1.2/3 (2 children) {
  │     Total:     33 bytes (33.33%, 1,650/Minstr) in 3 blocks (33.33%, 150/Minstr), avg size 11 bytes, avg lifetime 1,000 instrs (5% of program duration)
  │     At t-gmax: 33 bytes (33.33%) in 3 blocks (33.33%), avg size 11 bytes
  │     At t-end:  0 bytes (0%) in 0 blocks (0%), avg size 0 bytes
  │     Reads:     0 bytes (0%, 0/Minstr), 0/byte
  │     Writes:    0 bytes (0%, 0/Minstr), 0/byte
  │     Allocated at {
  │       #1: d()
  │       #2: e()
  │       #3: f()
  │     }
  │   }
  │   ├─▼ AP 1.2.1/2 (2 children) {
  │   │     Total:     23 bytes (23.23%, 1,150/Minstr) in 2 blocks (22.22%, 100/Minstr), avg size 11.5 bytes, avg lifetime 1,000 instrs (5% of program duration)
  │   │     At t-gmax: 23 bytes (23.23%) in 2 blocks (22.22%), avg size 11.5 bytes
  │   │     At t-end:  0 bytes (0%) in 0 blocks (0%), avg size 0 bytes
  │   │     Reads:     0 bytes (0%, 0/Minstr), 0/byte
  │   │     Writes:    0 bytes (0%, 0/Minstr), 0/byte
  │   │     Allocated at {
  │   │       ^1: d()
  │   │       ^2: e()
  │   │       ^3: f()
  │   │       #4: f()
  │   │     }
  │   │   }
  │   │   ├── AP 1.2.1.1/2 {
  │   │   │     Total:     12 bytes (12.12%, 600/Minstr) in 1 blocks (11.11%, 50/Minstr), avg size 12 bytes, avg lifetime 1,000 instrs (5% of program duration)
  │   │   │     Max:       12 bytes in 1 blocks, avg size 12 bytes
  │   │   │     At t-gmax: 12 bytes (12.12%) in 1 blocks (11.11%), avg size 12 bytes
  │   │   │     At t-end:  0 bytes (0%) in 0 blocks (0%), avg size 0 bytes
  │   │   │     Reads:     0 bytes (0%, 0/Minstr), 0/byte
  │   │   │     Writes:    0 bytes (0%, 0/Minstr), 0/byte
  │   │   │     Accesses: {
  │   │   │       [  0]  - - - - - - - - - - - - 
  │   │   │     }
  │   │   │     Allocated at {
  │   │   │       ^1: d()
  │   │   │       ^2: e()
  │   │   │       ^3: f()
  │   │   │       ^4: f()
  │   │   │       #5: f()
  │   │   │     }
  │   │   │   }
  │   │   └── AP 1.2.1.2/2 {
  │   │         Total:     11 bytes (11.11%, 550/Minstr) in 1 blocks (11.11%, 50/Minstr), avg size 11 bytes, avg lifetime 1,000 instrs (5% of program duration)
  │   │         Max:       11 bytes in 1 blocks, avg size 11 bytes
  │   │         At t-gmax: 11 bytes (11.11%) in 1 blocks (11.11%), avg size 11 bytes
  │   │         At t-end:  0 bytes (0%) in 0 blocks (0%), avg size 0 bytes
  │   │         Reads:     0 bytes (0%, 0/Minstr), 0/byte
  │   │         Writes:    0 bytes (0%, 0/Minstr), 0/byte
  │   │         Accesses: {
  │   │           [  0]  - - - - - - - - - - - 
  │   │         }
  │   │         Allocated at {
  │   │           ^1: d()
  │   │           ^2: e()
  │   │           ^3: f()
  │   │           ^4: f()
  │   │         }
  │   │       }
  │   └── AP 1.2.2/2 {
  │         Total:     10 bytes (10.1%, 500/Minstr) in 1 blocks (11.11%, 50/Minstr), avg size 10 bytes, avg lifetime 1,000 instrs (5% of program duration)
  │         Max:       10 bytes in 1 blocks, avg size 10 bytes
  │         At t-gmax: 10 bytes (10.1%) in 1 blocks (11.11%), avg size 10 bytes
  │         At t-end:  0 bytes (0%) in 0 blocks (0%), avg size 0 bytes
  │         Reads:     0 bytes (0%, 0/Minstr), 0/byte
  │         Writes:    0 bytes (0%, 0/Minstr), 0/byte
  │         Accesses: {
  │           [  0]  - - - - - - - - - - 
  │         }
  │         Allocated at {
  │           ^1: d()
  │           ^2: e()
  │           ^3: f()
  │         }
  │       }
  └─▼ AP 1.3/3 (3 children) {
        Total:     24 bytes (24.24%, 1,200/Minstr) in 3 blocks (33.33%, 150/Minstr), avg size 8 bytes, avg lifetime 1,000 instrs (5% of program duration)
        At t-gmax: 24 bytes (24.24%) in 3 blocks (33.33%), avg size 8 bytes
        At t-end:  0 bytes (0%) in 0 blocks (0%), avg size 0 bytes
        Reads:     0 bytes (0%, 0/Minstr), 0/byte
        Writes:    0 bytes (0%, 0/Minstr), 0/byte
        Allocated at {
          #1: g()
          #2: h()
        }
      }
      ├── AP 1.3.1/3 {
      │     Total:     9 bytes (9.09%, 450/Minstr) in 1 blocks (11.11%, 50/Minstr), avg size 9 bytes, avg lifetime 1,000 instrs (5% of program duration)
      │     Max:       9 bytes in 1 blocks, avg size 9 bytes
      │     At t-gmax: 9 bytes (9.09%) in 1 blocks (11.11%), avg size 9 bytes
      │     At t-end:  0 bytes (0%) in 0 blocks (0%), avg size 0 bytes
      │     Reads:     0 bytes (0%, 0/Minstr), 0/byte
      │     Writes:    0 bytes (0%, 0/Minstr), 0/byte
      │     Accesses: {
      │       [  0]  - - - - - - - - - 
      │     }
      │     Allocated at {
      │       ^1: g()
      │       ^2: h()
      │       #3: i()
      │     }
      │   }
      ├── AP 1.3.2/3 {
      │     Total:     8 bytes (8.08%, 400/Minstr) in 1 blocks (11.11%, 50/Minstr), avg size 8 bytes, avg lifetime 1,000 instrs (5% of program duration)
      │     Max:       8 bytes in 1 blocks, avg size 8 bytes
      │     At t-gmax: 8 bytes (8.08%) in 1 blocks (11.11%), avg size 8 bytes
      │     At t-end:  0 bytes (0%) in 0 blocks (0%), avg size 0 bytes
      │     Reads:     0 bytes (0%, 0/Minstr), 0/byte
      │     Writes:    0 bytes (0%, 0/Minstr), 0/byte
      │     Accesses: {
      │       [  0]  - - - - - - - - 
      │     }
      │     Allocated at {
      │       ^1: g()
      │       ^2: h()
      │       #3: j()
      │     }
      │   }
      └── AP 1.3.3/3 {
            Total:     7 bytes (7.07%, 350/Minstr) in 1 blocks (11.11%, 50/Minstr), avg size 7 bytes, avg lifetime 1,000 instrs (5% of program duration)
            Max:       7 bytes in 1 blocks, avg size 7 bytes
            At t-gmax: 7 bytes (7.07%) in 1 blocks (11.11%), avg size 7 bytes
            At t-end:  0 bytes (0%) in 0 blocks (0%), avg size 0 bytes
            Reads:     0 bytes (0%, 0/Minstr), 0/byte
            Writes:    0 bytes (0%, 0/Minstr), 0/byte
            Accesses: {
              [  0]  - - - - - - - 
            }
            Allocated at {
              ^1: g()
              ^2: h()
            }
          }

AP significance threshold: total >= 0.99 bytes (1%)
`
//---------------------------------------------------------------------------
    }
  ]
};
tests.push(subseqs);

//---------------------------------------------------------------------------
// acc (corresponds to dhat/tests/acc.c)
//---------------------------------------------------------------------------

let acc = {
  name: "acc",
  input:
//---------------------------------------------------------------------------
{"dhatFileVersion":1
,"cmd":"./acc"
,"pid":23513
,"mi":265120,"ei":1337753
,"aps":
 [{"tb":32,"tbk":1,"tli":4751
  ,"mb":32,"mbk":1
  ,"gb":0,"gbk":0
  ,"fb":0,"fbk":0
  ,"rb":0,"wb":496
  ,"acc":[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31]
  ,"fs":[1]
  }
 ,{"tb":20,"tbk":1,"tli":106
  ,"mb":20,"mbk":1
  ,"gb":0,"gbk":0
  ,"fb":0,"fbk":0
  ,"rb":4,"wb":48
  ,"acc":[-4,2,-4,0,-4,1,-4,0,-4,10]
  ,"fs":[2]
  }
 ,{"tb":33,"tbk":1,"tli":39
  ,"mb":33,"mbk":1
  ,"gb":0,"gbk":0
  ,"fb":0,"fbk":0
  ,"rb":0,"wb":1
  ,"acc":[-32,0,1]
  ,"fs":[3]
  }
 ,{"tb":1024,"tbk":1,"tli":15179
  ,"mb":1024,"mbk":1
  ,"gb":0,"gbk":0
  ,"fb":0,"fbk":0
  ,"rb":1024,"wb":1124
  ,"acc":[-500,2,-100,3,-424,2]
  ,"fs":[4]
  }
 ,{"tb":1025,"tbk":1,"tli":15415
  ,"mb":1025,"mbk":1
  ,"gb":1025,"gbk":1
  ,"fb":0,"fbk":0
  ,"rb":1025,"wb":1025
  ,"fs":[5]
  }
 ,{"tb":100,"tbk":1,"tli":350084
  ,"mb":100,"mbk":1
  ,"gb":0,"gbk":0
  ,"fb":0,"fbk":0
  ,"rb":0,"wb":200000
  ,"acc":[-4,50000,-96,0]
  ,"fs":[6,7]
  }
 ,{"tb":100,"tbk":1,"tli":350072
  ,"mb":100,"mbk":1
  ,"gb":0,"gbk":0
  ,"fb":0,"fbk":0
  ,"rb":0,"wb":200000
  ,"acc":[-4,50000,-96,0]
  ,"fs":[6,8]
  }
 ,{"tb":100,"tbk":1,"tli":700084
  ,"mb":100,"mbk":1
  ,"gb":0,"gbk":0
  ,"fb":0,"fbk":0
  ,"rb":0,"wb":400000
  ,"acc":[-4,65535,-96,0]
  ,"fs":[9,10]
  }
 ,{"tb":100,"tbk":1,"tli":700072
  ,"mb":100,"mbk":1
  ,"gb":0,"gbk":0
  ,"fb":0,"fbk":0
  ,"rb":0,"wb":400000
  ,"acc":[-4,65535,-96,0]
  ,"fs":[9,11]
  }
 ]
,"ftbl":
 ["[root]"
 ,"0x10871F: main (acc.c:14)"
 ,"0x108771: main (acc.c:23)"
 ,"0x1087CB: main (acc.c:32)"
 ,"0x1087F0: main (acc.c:37)"
 ,"0x10886F: main (acc.c:47)"
 ,"0x1086F1: m1 (acc.c:7)"
 ,"0x1088C3: main (acc.c:54)"
 ,"0x1088D1: main (acc.c:55)"
 ,"0x10870B: m2 (acc.c:9)"
 ,"0x108921: main (acc.c:64)"
 ,"0x10892F: main (acc.c:65)"
 ]
}
//---------------------------------------------------------------------------
  ,
  outputs: [
    {
      // All blocks are freed, which means all "At t-end" values are 0, so
      // sorting by atTEndBytes results in no aggregate nodes, which is what we
      // want here.
      label: "At t-end (bytes)",
      expected:
//---------------------------------------------------------------------------
`\
Invocation {
  Command: ./acc
  PID:     23513
}

Times {
  t-gmax: 265,120 instrs (19.82% of program duration)
  t-end:  1,337,753 instrs
}

▼ AP 1/1 (7 children) {
    Total:     2,534 bytes (100%, 1,894.22/Minstr) in 9 blocks (100%, 6.73/Minstr), avg size 281.56 bytes, avg lifetime 237,311.33 instrs (17.74% of program duration)
    At t-gmax: 1,025 bytes (100%) in 1 blocks (100%), avg size 1,025 bytes
    At t-end:  0 bytes (0%) in 0 blocks (0%), avg size 0 bytes
    Reads:     2,053 bytes (100%, 1,534.66/Minstr), 0.81/byte
    Writes:    1,202,694 bytes (100%, 899,040.41/Minstr), 474.62/byte
    Allocated at {
      #0: [root]
    }
  }
  ├── AP 1.1/7 {
  │     Total:     1,025 bytes (40.45%, 766.21/Minstr) in 1 blocks (11.11%, 0.75/Minstr), avg size 1,025 bytes, avg lifetime 15,415 instrs (1.15% of program duration)
  │     Max:       1,025 bytes in 1 blocks, avg size 1,025 bytes
  │     At t-gmax: 1,025 bytes (100%) in 1 blocks (100%), avg size 1,025 bytes
  │     At t-end:  0 bytes (0%) in 0 blocks (0%), avg size 0 bytes
  │     Reads:     1,025 bytes (49.93%, 766.21/Minstr), 1/byte
  │     Writes:    1,025 bytes (0.09%, 766.21/Minstr), 1/byte
  │     Allocated at {
  │       #1: 0x10886F: main (acc.c:47)
  │     }
  │   }
  ├── AP 1.2/7 {
  │     Total:     1,024 bytes (40.41%, 765.46/Minstr) in 1 blocks (11.11%, 0.75/Minstr), avg size 1,024 bytes, avg lifetime 15,179 instrs (1.13% of program duration)
  │     Max:       1,024 bytes in 1 blocks, avg size 1,024 bytes
  │     At t-gmax: 0 bytes (0%) in 0 blocks (0%), avg size 0 bytes
  │     At t-end:  0 bytes (0%) in 0 blocks (0%), avg size 0 bytes
  │     Reads:     1,024 bytes (49.88%, 765.46/Minstr), 1/byte
  │     Writes:    1,124 bytes (0.09%, 840.21/Minstr), 1.1/byte
  │     Accesses: {
  │       [  0]  2 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 
  │       [ 32]  2 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 
  │       [ 64]  2 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 
  │       [ 96]  2 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 
  │       [128]  2 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 
  │       [160]  2 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 
  │       [192]  2 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 
  │       [224]  2 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 
  │       [256]  2 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 
  │       [288]  2 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 
  │       [320]  2 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 
  │       [352]  2 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 
  │       [384]  2 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 
  │       [416]  2 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 
  │       [448]  2 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 
  │       [480]  2 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 3 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 
  │       [512]  3 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 
  │       [544]  3 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 
  │       [576]  3 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 2 〃 〃 〃 〃 〃 〃 〃 
  │       [608]  2 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 
  │       [640]  2 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 
  │       [672]  2 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 
  │       [704]  2 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 
  │       [736]  2 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 
  │       [768]  2 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 
  │       [800]  2 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 
  │       [832]  2 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 
  │       [864]  2 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 
  │       [896]  2 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 
  │       [928]  2 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 
  │       [960]  2 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 
  │       [992]  2 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 
  │     }
  │     Allocated at {
  │       #1: 0x1087F0: main (acc.c:37)
  │     }
  │   }
  ├─▼ AP 1.3/7 (2 children) {
  │     Total:     200 bytes (7.89%, 149.5/Minstr) in 2 blocks (22.22%, 1.5/Minstr), avg size 100 bytes, avg lifetime 350,078 instrs (26.17% of program duration)
  │     At t-gmax: 0 bytes (0%) in 0 blocks (0%), avg size 0 bytes
  │     At t-end:  0 bytes (0%) in 0 blocks (0%), avg size 0 bytes
  │     Reads:     0 bytes (0%, 0/Minstr), 0/byte
  │     Writes:    400,000 bytes (33.26%, 299,008.86/Minstr), 2,000/byte
  │     Accesses: {
  │       [  0]  100000 〃 〃 〃 - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  │       [ 32]  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  │       [ 64]  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  │       [ 96]  - - - - 
  │     }
  │     Allocated at {
  │       #1: 0x1086F1: m1 (acc.c:7)
  │     }
  │   }
  │   ├── AP 1.3.1/2 {
  │   │     Total:     100 bytes (3.95%, 74.75/Minstr) in 1 blocks (11.11%, 0.75/Minstr), avg size 100 bytes, avg lifetime 350,084 instrs (26.17% of program duration)
  │   │     Max:       100 bytes in 1 blocks, avg size 100 bytes
  │   │     At t-gmax: 0 bytes (0%) in 0 blocks (0%), avg size 0 bytes
  │   │     At t-end:  0 bytes (0%) in 0 blocks (0%), avg size 0 bytes
  │   │     Reads:     0 bytes (0%, 0/Minstr), 0/byte
  │   │     Writes:    200,000 bytes (16.63%, 149,504.43/Minstr), 2,000/byte
  │   │     Accesses: {
  │   │       [  0]  50000 〃 〃 〃 - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  │   │       [ 32]  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  │   │       [ 64]  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  │   │       [ 96]  - - - - 
  │   │     }
  │   │     Allocated at {
  │   │       ^1: 0x1086F1: m1 (acc.c:7)
  │   │       #2: 0x1088C3: main (acc.c:54)
  │   │     }
  │   │   }
  │   └── AP 1.3.2/2 {
  │         Total:     100 bytes (3.95%, 74.75/Minstr) in 1 blocks (11.11%, 0.75/Minstr), avg size 100 bytes, avg lifetime 350,072 instrs (26.17% of program duration)
  │         Max:       100 bytes in 1 blocks, avg size 100 bytes
  │         At t-gmax: 0 bytes (0%) in 0 blocks (0%), avg size 0 bytes
  │         At t-end:  0 bytes (0%) in 0 blocks (0%), avg size 0 bytes
  │         Reads:     0 bytes (0%, 0/Minstr), 0/byte
  │         Writes:    200,000 bytes (16.63%, 149,504.43/Minstr), 2,000/byte
  │         Accesses: {
  │           [  0]  50000 〃 〃 〃 - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  │           [ 32]  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  │           [ 64]  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  │           [ 96]  - - - - 
  │         }
  │         Allocated at {
  │           ^1: 0x1086F1: m1 (acc.c:7)
  │           #2: 0x1088D1: main (acc.c:55)
  │         }
  │       }
  ├─▼ AP 1.4/7 (2 children) {
  │     Total:     200 bytes (7.89%, 149.5/Minstr) in 2 blocks (22.22%, 1.5/Minstr), avg size 100 bytes, avg lifetime 700,078 instrs (52.33% of program duration)
  │     At t-gmax: 0 bytes (0%) in 0 blocks (0%), avg size 0 bytes
  │     At t-end:  0 bytes (0%) in 0 blocks (0%), avg size 0 bytes
  │     Reads:     0 bytes (0%, 0/Minstr), 0/byte
  │     Writes:    800,000 bytes (66.52%, 598,017.72/Minstr), 4,000/byte
  │     Accesses: {
  │       [  0]  ∞ 〃 〃 〃 - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  │       [ 32]  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  │       [ 64]  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  │       [ 96]  - - - - 
  │     }
  │     Allocated at {
  │       #1: 0x10870B: m2 (acc.c:9)
  │     }
  │   }
  │   ├── AP 1.4.1/2 {
  │   │     Total:     100 bytes (3.95%, 74.75/Minstr) in 1 blocks (11.11%, 0.75/Minstr), avg size 100 bytes, avg lifetime 700,084 instrs (52.33% of program duration)
  │   │     Max:       100 bytes in 1 blocks, avg size 100 bytes
  │   │     At t-gmax: 0 bytes (0%) in 0 blocks (0%), avg size 0 bytes
  │   │     At t-end:  0 bytes (0%) in 0 blocks (0%), avg size 0 bytes
  │   │     Reads:     0 bytes (0%, 0/Minstr), 0/byte
  │   │     Writes:    400,000 bytes (33.26%, 299,008.86/Minstr), 4,000/byte
  │   │     Accesses: {
  │   │       [  0]  ∞ 〃 〃 〃 - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  │   │       [ 32]  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  │   │       [ 64]  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  │   │       [ 96]  - - - - 
  │   │     }
  │   │     Allocated at {
  │   │       ^1: 0x10870B: m2 (acc.c:9)
  │   │       #2: 0x108921: main (acc.c:64)
  │   │     }
  │   │   }
  │   └── AP 1.4.2/2 {
  │         Total:     100 bytes (3.95%, 74.75/Minstr) in 1 blocks (11.11%, 0.75/Minstr), avg size 100 bytes, avg lifetime 700,072 instrs (52.33% of program duration)
  │         Max:       100 bytes in 1 blocks, avg size 100 bytes
  │         At t-gmax: 0 bytes (0%) in 0 blocks (0%), avg size 0 bytes
  │         At t-end:  0 bytes (0%) in 0 blocks (0%), avg size 0 bytes
  │         Reads:     0 bytes (0%, 0/Minstr), 0/byte
  │         Writes:    400,000 bytes (33.26%, 299,008.86/Minstr), 4,000/byte
  │         Accesses: {
  │           [  0]  ∞ 〃 〃 〃 - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  │           [ 32]  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  │           [ 64]  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  │           [ 96]  - - - - 
  │         }
  │         Allocated at {
  │           ^1: 0x10870B: m2 (acc.c:9)
  │           #2: 0x10892F: main (acc.c:65)
  │         }
  │       }
  ├── AP 1.5/7 {
  │     Total:     33 bytes (1.3%, 24.67/Minstr) in 1 blocks (11.11%, 0.75/Minstr), avg size 33 bytes, avg lifetime 39 instrs (0% of program duration)
  │     Max:       33 bytes in 1 blocks, avg size 33 bytes
  │     At t-gmax: 0 bytes (0%) in 0 blocks (0%), avg size 0 bytes
  │     At t-end:  0 bytes (0%) in 0 blocks (0%), avg size 0 bytes
  │     Reads:     0 bytes (0%, 0/Minstr), 0/byte
  │     Writes:    1 bytes (0%, 0.75/Minstr), 0.03/byte
  │     Accesses: {
  │       [  0]  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  │       [ 32]  1 
  │     }
  │     Allocated at {
  │       #1: 0x1087CB: main (acc.c:32)
  │     }
  │   }
  ├── AP 1.6/7 {
  │     Total:     32 bytes (1.26%, 23.92/Minstr) in 1 blocks (11.11%, 0.75/Minstr), avg size 32 bytes, avg lifetime 4,751 instrs (0.36% of program duration)
  │     Max:       32 bytes in 1 blocks, avg size 32 bytes
  │     At t-gmax: 0 bytes (0%) in 0 blocks (0%), avg size 0 bytes
  │     At t-end:  0 bytes (0%) in 0 blocks (0%), avg size 0 bytes
  │     Reads:     0 bytes (0%, 0/Minstr), 0/byte
  │     Writes:    496 bytes (0.04%, 370.77/Minstr), 15.5/byte
  │     Accesses: {
  │       [  0]  - 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 
  │     }
  │     Allocated at {
  │       #1: 0x10871F: main (acc.c:14)
  │     }
  │   }
  └── AP 1.7/7 {
        Total:     20 bytes (0.79%, 14.95/Minstr) in 1 blocks (11.11%, 0.75/Minstr), avg size 20 bytes, avg lifetime 106 instrs (0.01% of program duration)
        Max:       20 bytes in 1 blocks, avg size 20 bytes
        At t-gmax: 0 bytes (0%) in 0 blocks (0%), avg size 0 bytes
        At t-end:  0 bytes (0%) in 0 blocks (0%), avg size 0 bytes
        Reads:     4 bytes (0.19%, 2.99/Minstr), 0.2/byte
        Writes:    48 bytes (0%, 35.88/Minstr), 2.4/byte
        Accesses: {
          [  0]  2 〃 〃 〃 - - - - 1 〃 〃 〃 - - - - 10 〃 〃 〃 
        }
        Allocated at {
          #1: 0x108771: main (acc.c:23)
        }
      }

AP significance threshold: at-t-end >= 0 bytes (0%)
`
//---------------------------------------------------------------------------
    }
  ]
};
tests.push(acc);

//---------------------------------------------------------------------------
// big (corresponds to dhat/tests/big.c)
//---------------------------------------------------------------------------

let big = {
  name: "big",
  input:
//---------------------------------------------------------------------------
{"dhatFileVersion":1
,"cmd":"./big"
,"pid":3902
,"mi":245281,"ei":253354
,"aps":
 [{"tb":706,"tbk":1,"tli":543
  ,"mb":706,"mbk":1
  ,"gb":706,"gbk":1
  ,"fb":0,"fbk":0
  ,"rb":0,"wb":0
  ,"acc":[-706,0]
  ,"fs":[1,2,3,4,5]
  }
 ,{"tb":5,"tbk":1,"tli":7972
  ,"mb":5,"mbk":1
  ,"gb":0,"gbk":0
  ,"fb":5,"fbk":1
  ,"rb":0,"wb":0
  ,"acc":[-5,0]
  ,"fs":[1,2,3,6,7]
  }
 ,{"tb":30,"tbk":1,"tli":7910
  ,"mb":30,"mbk":1
  ,"gb":0,"gbk":0
  ,"fb":30,"fbk":1
  ,"rb":0,"wb":0
  ,"acc":[-30,0]
  ,"fs":[1,2,8,9]
  }
 ,{"tb":20,"tbk":1,"tli":7857
  ,"mb":20,"mbk":1
  ,"gb":0,"gbk":0
  ,"fb":20,"fbk":1
  ,"rb":0,"wb":0
  ,"acc":[-20,0]
  ,"fs":[1,10,11]
  }
 ,{"tb":10,"tbk":1,"tli":7792
  ,"mb":10,"mbk":1
  ,"gb":0,"gbk":0
  ,"fb":10,"fbk":1
  ,"rb":0,"wb":0
  ,"acc":[-10,0]
  ,"fs":[1,12,13,14,15]
  }
 ,{"tb":60,"tbk":1,"tli":7709
  ,"mb":60,"mbk":1
  ,"gb":0,"gbk":0
  ,"fb":60,"fbk":1
  ,"rb":0,"wb":0
  ,"acc":[-60,0]
  ,"fs":[16,17,18,19,20,21,22]
  }
 ,{"tb":30,"tbk":1,"tli":7622
  ,"mb":30,"mbk":1
  ,"gb":0,"gbk":0
  ,"fb":30,"fbk":1
  ,"rb":0,"wb":0
  ,"acc":[-30,0]
  ,"fs":[16,17,18,23,24,25,26]
  }
 ,{"tb":20,"tbk":1,"tli":7528
  ,"mb":20,"mbk":1
  ,"gb":0,"gbk":0
  ,"fb":20,"fbk":1
  ,"rb":0,"wb":0
  ,"acc":[-20,0]
  ,"fs":[16,17,18,23,24,27,28,29]
  }
 ,{"tb":7,"tbk":1,"tli":7446
  ,"mb":7,"mbk":1
  ,"gb":0,"gbk":0
  ,"fb":7,"fbk":1
  ,"rb":0,"wb":0
  ,"acc":[-7,0]
  ,"fs":[16,17,18,30,31,32]
  }
 ,{"tb":3,"tbk":1,"tli":7375
  ,"mb":3,"mbk":1
  ,"gb":0,"gbk":0
  ,"fb":3,"fbk":1
  ,"rb":0,"wb":0
  ,"acc":[-3,0]
  ,"fs":[16,17,18,33,34]
  }
 ,{"tb":30,"tbk":1,"tli":7299
  ,"mb":30,"mbk":1
  ,"gb":0,"gbk":0
  ,"fb":30,"fbk":1
  ,"rb":0,"wb":0
  ,"acc":[-30,0]
  ,"fs":[35,36,37,38,39,40]
  }
 ,{"tb":20,"tbk":1,"tli":7249
  ,"mb":20,"mbk":1
  ,"gb":0,"gbk":0
  ,"fb":20,"fbk":1
  ,"rb":0,"wb":0
  ,"acc":[-20,0]
  ,"fs":[41,42]
  }
 ,{"tb":19,"tbk":1,"tli":7207
  ,"mb":19,"mbk":1
  ,"gb":0,"gbk":0
  ,"fb":19,"fbk":1
  ,"rb":0,"wb":0
  ,"acc":[-19,0]
  ,"fs":[43,44]
  }
 ,{"tb":9,"tbk":1,"tli":7158
  ,"mb":9,"mbk":1
  ,"gb":0,"gbk":0
  ,"fb":9,"fbk":1
  ,"rb":0,"wb":0
  ,"acc":[-9,0]
  ,"fs":[45,46,47]
  }
 ,{"tb":8,"tbk":1,"tli":7107
  ,"mb":8,"mbk":1
  ,"gb":0,"gbk":0
  ,"fb":8,"fbk":1
  ,"rb":0,"wb":0
  ,"acc":[-8,0]
  ,"fs":[45,48,49]
  }
 ,{"tb":7,"tbk":1,"tli":7056
  ,"mb":7,"mbk":1
  ,"gb":0,"gbk":0
  ,"fb":7,"fbk":1
  ,"rb":0,"wb":0
  ,"acc":[-7,0]
  ,"fs":[45,50,51]
  }
 ,{"tb":5,"tbk":1,"tli":7005
  ,"mb":5,"mbk":1
  ,"gb":0,"gbk":0
  ,"fb":5,"fbk":1
  ,"rb":0,"wb":0
  ,"acc":[-5,0]
  ,"fs":[45,52,53]
  }
 ,{"tb":1,"tbk":1,"tli":6954
  ,"mb":1,"mbk":1
  ,"gb":0,"gbk":0
  ,"fb":1,"fbk":1
  ,"rb":0,"wb":0
  ,"acc":[0]
  ,"fs":[45,52,54]
  }
 ,{"tb":10,"tbk":1,"tli":6917
  ,"mb":10,"mbk":1
  ,"gb":0,"gbk":0
  ,"fb":10,"fbk":1
  ,"rb":0,"wb":0
  ,"acc":[-10,0]
  ,"fs":[55]
  }
 ]
,"ftbl":
 ["[root]"
 ,"0x1086A1: a (big.c:10)"
 ,"0x1086BB: b1 (big.c:11)"
 ,"0x1086D5: c1 (big.c:12)"
 ,"0x1086EF: d1 (big.c:13)"
 ,"0x108A43: main (big.c:38)"
 ,"0x108709: d2 (big.c:14)"
 ,"0x108A5D: main (big.c:41)"
 ,"0x108723: c2 (big.c:15)"
 ,"0x108A67: main (big.c:42)"
 ,"0x10873D: b2 (big.c:16)"
 ,"0x108A71: main (big.c:43)"
 ,"0x108757: b3 (big.c:17)"
 ,"0x108771: e (big.c:17)"
 ,"0x10878B: f (big.c:17)"
 ,"0x108A7B: main (big.c:44)"
 ,"0x1087A5: g (big.c:18)"
 ,"0x1087BF: h (big.c:18)"
 ,"0x1087D9: i (big.c:18)"
 ,"0x1087F3: j2 (big.c:19)"
 ,"0x10880D: k (big.c:19)"
 ,"0x108827: l (big.c:19)"
 ,"0x108A85: main (big.c:45)"
 ,"0x108841: j3 (big.c:20)"
 ,"0x10885B: m (big.c:20)"
 ,"0x108875: n1 (big.c:21)"
 ,"0x108A8F: main (big.c:46)"
 ,"0x10888F: n2 (big.c:22)"
 ,"0x1088A9: o (big.c:22)"
 ,"0x108A99: main (big.c:47)"
 ,"0x1088C3: p (big.c:23)"
 ,"0x1088DD: q (big.c:23)"
 ,"0x108AA3: main (big.c:48)"
 ,"0x1088F7: r (big.c:24)"
 ,"0x108AAD: main (big.c:49)"
 ,"0x108911: s1 (big.c:25)"
 ,"0x10892B: s2 (big.c:25)"
 ,"0x108945: s3 (big.c:25)"
 ,"0x10895F: s4 (big.c:25)"
 ,"0x108979: s5 (big.c:25)"
 ,"0x108AB7: main (big.c:50)"
 ,"0x108993: t (big.c:26)"
 ,"0x108AC1: main (big.c:51)"
 ,"0x1089AD: u (big.c:27)"
 ,"0x108ACB: main (big.c:52)"
 ,"0x1089C7: v (big.c:28)"
 ,"0x1089E1: w (big.c:29)"
 ,"0x108AD5: main (big.c:53)"
 ,"0x1089FB: x (big.c:30)"
 ,"0x108ADF: main (big.c:54)"
 ,"0x108A15: y (big.c:31)"
 ,"0x108AE9: main (big.c:55)"
 ,"0x108A2F: z (big.c:32)"
 ,"0x108AF3: main (big.c:56)"
 ,"0x108AFD: main (big.c:57)"
 ,"0x108B07: main (big.c:60)"
 ]
}
//---------------------------------------------------------------------------
  ,
  outputs: [
    {
      label: "Total (bytes)",
      expected:
//---------------------------------------------------------------------------
`\
Invocation {
  Command: ./big
  PID:     3902
}

Times {
  t-gmax: 245,281 instrs (96.81% of program duration)
  t-end:  253,354 instrs
}

▼ AP 1/1 (7 children) {
    Total:     1,000 bytes (100%, 3,947.05/Minstr) in 19 blocks (100%, 74.99/Minstr), avg size 52.63 bytes, avg lifetime 7,037.16 instrs (2.78% of program duration)
    At t-gmax: 706 bytes (100%) in 1 blocks (100%), avg size 706 bytes
    At t-end:  294 bytes (100%) in 18 blocks (100%), avg size 16.33 bytes
    Reads:     0 bytes (0%, 0/Minstr), 0/byte
    Writes:    0 bytes (0%, 0/Minstr), 0/byte
    Allocated at {
      #0: [root]
    }
  }
  ├─▼ AP 1.1/7 (3 children) {
  │     Total:     771 bytes (77.1%, 3,043.17/Minstr) in 5 blocks (26.32%, 19.74/Minstr), avg size 154.2 bytes, avg lifetime 6,414.8 instrs (2.53% of program duration)
  │     At t-gmax: 706 bytes (100%) in 1 blocks (100%), avg size 706 bytes
  │     At t-end:  65 bytes (22.11%) in 4 blocks (22.22%), avg size 16.25 bytes
  │     Reads:     0 bytes (0%, 0/Minstr), 0/byte
  │     Writes:    0 bytes (0%, 0/Minstr), 0/byte
  │     Allocated at {
  │       #1: 0x1086A1: a (big.c:10)
  │     }
  │   }
  │   ├─▼ AP 1.1.1/3 (2 children) {
  │   │     Total:     741 bytes (74.1%, 2,924.76/Minstr) in 3 blocks (15.79%, 11.84/Minstr), avg size 247 bytes, avg lifetime 5,475 instrs (2.16% of program duration)
  │   │     At t-gmax: 706 bytes (100%) in 1 blocks (100%), avg size 706 bytes
  │   │     At t-end:  35 bytes (11.9%) in 2 blocks (11.11%), avg size 17.5 bytes
  │   │     Reads:     0 bytes (0%, 0/Minstr), 0/byte
  │   │     Writes:    0 bytes (0%, 0/Minstr), 0/byte
  │   │     Allocated at {
  │   │       ^1: 0x1086A1: a (big.c:10)
  │   │       #2: 0x1086BB: b1 (big.c:11)
  │   │     }
  │   │   }
  │   │   ├─▼ AP 1.1.1.1/2 (2 children) {
  │   │   │     Total:     711 bytes (71.1%, 2,806.35/Minstr) in 2 blocks (10.53%, 7.89/Minstr), avg size 355.5 bytes, avg lifetime 4,257.5 instrs (1.68% of program duration)
  │   │   │     At t-gmax: 706 bytes (100%) in 1 blocks (100%), avg size 706 bytes
  │   │   │     At t-end:  5 bytes (1.7%) in 1 blocks (5.56%), avg size 5 bytes
  │   │   │     Reads:     0 bytes (0%, 0/Minstr), 0/byte
  │   │   │     Writes:    0 bytes (0%, 0/Minstr), 0/byte
  │   │   │     Allocated at {
  │   │   │       ^1: 0x1086A1: a (big.c:10)
  │   │   │       ^2: 0x1086BB: b1 (big.c:11)
  │   │   │       #3: 0x1086D5: c1 (big.c:12)
  │   │   │     }
  │   │   │   }
  │   │   │   ├── AP 1.1.1.1.1/2 {
  │   │   │   │     Total:     706 bytes (70.6%, 2,786.61/Minstr) in 1 blocks (5.26%, 3.95/Minstr), avg size 706 bytes, avg lifetime 543 instrs (0.21% of program duration)
  │   │   │   │     Max:       706 bytes in 1 blocks, avg size 706 bytes
  │   │   │   │     At t-gmax: 706 bytes (100%) in 1 blocks (100%), avg size 706 bytes
  │   │   │   │     At t-end:  0 bytes (0%) in 0 blocks (0%), avg size 0 bytes
  │   │   │   │     Reads:     0 bytes (0%, 0/Minstr), 0/byte
  │   │   │   │     Writes:    0 bytes (0%, 0/Minstr), 0/byte
  │   │   │   │     Accesses: {
  │   │   │   │       [  0]  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  │   │   │   │       [ 32]  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  │   │   │   │       [ 64]  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  │   │   │   │       [ 96]  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  │   │   │   │       [128]  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  │   │   │   │       [160]  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  │   │   │   │       [192]  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  │   │   │   │       [224]  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  │   │   │   │       [256]  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  │   │   │   │       [288]  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  │   │   │   │       [320]  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  │   │   │   │       [352]  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  │   │   │   │       [384]  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  │   │   │   │       [416]  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  │   │   │   │       [448]  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  │   │   │   │       [480]  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  │   │   │   │       [512]  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  │   │   │   │       [544]  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  │   │   │   │       [576]  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  │   │   │   │       [608]  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  │   │   │   │       [640]  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  │   │   │   │       [672]  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  │   │   │   │       [704]  - - 
  │   │   │   │     }
  │   │   │   │     Allocated at {
  │   │   │   │       ^1: 0x1086A1: a (big.c:10)
  │   │   │   │       ^2: 0x1086BB: b1 (big.c:11)
  │   │   │   │       ^3: 0x1086D5: c1 (big.c:12)
  │   │   │   │       #4: 0x1086EF: d1 (big.c:13)
  │   │   │   │       #5: 0x108A43: main (big.c:38)
  │   │   │   │     }
  │   │   │   │   }
  │   │   │   └── AP 1.1.1.1.2/2 {
  │   │   │         Total:     5 bytes (0.5%, 19.74/Minstr)
  │   │   │         Allocated at {
  │   │   │           [1 insignificant]
  │   │   │         }
  │   │   │       }
  │   │   └── AP 1.1.1.2/2 {
  │   │         Total:     30 bytes (3%, 118.41/Minstr) in 1 blocks (5.26%, 3.95/Minstr), avg size 30 bytes, avg lifetime 7,910 instrs (3.12% of program duration)
  │   │         Max:       30 bytes in 1 blocks, avg size 30 bytes
  │   │         At t-gmax: 0 bytes (0%) in 0 blocks (0%), avg size 0 bytes
  │   │         At t-end:  30 bytes (10.2%) in 1 blocks (5.56%), avg size 30 bytes
  │   │         Reads:     0 bytes (0%, 0/Minstr), 0/byte
  │   │         Writes:    0 bytes (0%, 0/Minstr), 0/byte
  │   │         Accesses: {
  │   │           [  0]  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  │   │         }
  │   │         Allocated at {
  │   │           ^1: 0x1086A1: a (big.c:10)
  │   │           ^2: 0x1086BB: b1 (big.c:11)
  │   │           #3: 0x108723: c2 (big.c:15)
  │   │           #4: 0x108A67: main (big.c:42)
  │   │         }
  │   │       }
  │   ├── AP 1.1.2/3 {
  │   │     Total:     20 bytes (2%, 78.94/Minstr) in 1 blocks (5.26%, 3.95/Minstr), avg size 20 bytes, avg lifetime 7,857 instrs (3.1% of program duration)
  │   │     Max:       20 bytes in 1 blocks, avg size 20 bytes
  │   │     At t-gmax: 0 bytes (0%) in 0 blocks (0%), avg size 0 bytes
  │   │     At t-end:  20 bytes (6.8%) in 1 blocks (5.56%), avg size 20 bytes
  │   │     Reads:     0 bytes (0%, 0/Minstr), 0/byte
  │   │     Writes:    0 bytes (0%, 0/Minstr), 0/byte
  │   │     Accesses: {
  │   │       [  0]  - - - - - - - - - - - - - - - - - - - - 
  │   │     }
  │   │     Allocated at {
  │   │       ^1: 0x1086A1: a (big.c:10)
  │   │       #2: 0x10873D: b2 (big.c:16)
  │   │       #3: 0x108A71: main (big.c:43)
  │   │     }
  │   │   }
  │   └── AP 1.1.3/3 {
  │         Total:     10 bytes (1%, 39.47/Minstr) in 1 blocks (5.26%, 3.95/Minstr), avg size 10 bytes, avg lifetime 7,792 instrs (3.08% of program duration)
  │         Max:       10 bytes in 1 blocks, avg size 10 bytes
  │         At t-gmax: 0 bytes (0%) in 0 blocks (0%), avg size 0 bytes
  │         At t-end:  10 bytes (3.4%) in 1 blocks (5.56%), avg size 10 bytes
  │         Reads:     0 bytes (0%, 0/Minstr), 0/byte
  │         Writes:    0 bytes (0%, 0/Minstr), 0/byte
  │         Accesses: {
  │           [  0]  - - - - - - - - - - 
  │         }
  │         Allocated at {
  │           ^1: 0x1086A1: a (big.c:10)
  │           #2: 0x108757: b3 (big.c:17)
  │           #3: 0x108771: e (big.c:17)
  │           #4: 0x10878B: f (big.c:17)
  │           #5: 0x108A7B: main (big.c:44)
  │         }
  │       }
  ├─▼ AP 1.2/7 (3 children) {
  │     Total:     120 bytes (12%, 473.65/Minstr) in 5 blocks (26.32%, 19.74/Minstr), avg size 24 bytes, avg lifetime 7,536 instrs (2.97% of program duration)
  │     At t-gmax: 0 bytes (0%) in 0 blocks (0%), avg size 0 bytes
  │     At t-end:  120 bytes (40.82%) in 5 blocks (27.78%), avg size 24 bytes
  │     Reads:     0 bytes (0%, 0/Minstr), 0/byte
  │     Writes:    0 bytes (0%, 0/Minstr), 0/byte
  │     Allocated at {
  │       #1: 0x1087A5: g (big.c:18)
  │       #2: 0x1087BF: h (big.c:18)
  │       #3: 0x1087D9: i (big.c:18)
  │     }
  │   }
  │   ├── AP 1.2.1/3 {
  │   │     Total:     60 bytes (6%, 236.82/Minstr) in 1 blocks (5.26%, 3.95/Minstr), avg size 60 bytes, avg lifetime 7,709 instrs (3.04% of program duration)
  │   │     Max:       60 bytes in 1 blocks, avg size 60 bytes
  │   │     At t-gmax: 0 bytes (0%) in 0 blocks (0%), avg size 0 bytes
  │   │     At t-end:  60 bytes (20.41%) in 1 blocks (5.56%), avg size 60 bytes
  │   │     Reads:     0 bytes (0%, 0/Minstr), 0/byte
  │   │     Writes:    0 bytes (0%, 0/Minstr), 0/byte
  │   │     Accesses: {
  │   │       [  0]  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  │   │       [ 32]  - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  │   │     }
  │   │     Allocated at {
  │   │       ^1: 0x1087A5: g (big.c:18)
  │   │       ^2: 0x1087BF: h (big.c:18)
  │   │       ^3: 0x1087D9: i (big.c:18)
  │   │       #4: 0x1087F3: j2 (big.c:19)
  │   │       #5: 0x10880D: k (big.c:19)
  │   │       #6: 0x108827: l (big.c:19)
  │   │       #7: 0x108A85: main (big.c:45)
  │   │     }
  │   │   }
  │   ├─▼ AP 1.2.2/3 (2 children) {
  │   │     Total:     50 bytes (5%, 197.35/Minstr) in 2 blocks (10.53%, 7.89/Minstr), avg size 25 bytes, avg lifetime 7,575 instrs (2.99% of program duration)
  │   │     At t-gmax: 0 bytes (0%) in 0 blocks (0%), avg size 0 bytes
  │   │     At t-end:  50 bytes (17.01%) in 2 blocks (11.11%), avg size 25 bytes
  │   │     Reads:     0 bytes (0%, 0/Minstr), 0/byte
  │   │     Writes:    0 bytes (0%, 0/Minstr), 0/byte
  │   │     Allocated at {
  │   │       ^1: 0x1087A5: g (big.c:18)
  │   │       ^2: 0x1087BF: h (big.c:18)
  │   │       ^3: 0x1087D9: i (big.c:18)
  │   │       #4: 0x108841: j3 (big.c:20)
  │   │       #5: 0x10885B: m (big.c:20)
  │   │     }
  │   │   }
  │   │   ├── AP 1.2.2.1/2 {
  │   │   │     Total:     30 bytes (3%, 118.41/Minstr) in 1 blocks (5.26%, 3.95/Minstr), avg size 30 bytes, avg lifetime 7,622 instrs (3.01% of program duration)
  │   │   │     Max:       30 bytes in 1 blocks, avg size 30 bytes
  │   │   │     At t-gmax: 0 bytes (0%) in 0 blocks (0%), avg size 0 bytes
  │   │   │     At t-end:  30 bytes (10.2%) in 1 blocks (5.56%), avg size 30 bytes
  │   │   │     Reads:     0 bytes (0%, 0/Minstr), 0/byte
  │   │   │     Writes:    0 bytes (0%, 0/Minstr), 0/byte
  │   │   │     Accesses: {
  │   │   │       [  0]  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  │   │   │     }
  │   │   │     Allocated at {
  │   │   │       ^1: 0x1087A5: g (big.c:18)
  │   │   │       ^2: 0x1087BF: h (big.c:18)
  │   │   │       ^3: 0x1087D9: i (big.c:18)
  │   │   │       ^4: 0x108841: j3 (big.c:20)
  │   │   │       ^5: 0x10885B: m (big.c:20)
  │   │   │       #6: 0x108875: n1 (big.c:21)
  │   │   │       #7: 0x108A8F: main (big.c:46)
  │   │   │     }
  │   │   │   }
  │   │   └── AP 1.2.2.2/2 {
  │   │         Total:     20 bytes (2%, 78.94/Minstr) in 1 blocks (5.26%, 3.95/Minstr), avg size 20 bytes, avg lifetime 7,528 instrs (2.97% of program duration)
  │   │         Max:       20 bytes in 1 blocks, avg size 20 bytes
  │   │         At t-gmax: 0 bytes (0%) in 0 blocks (0%), avg size 0 bytes
  │   │         At t-end:  20 bytes (6.8%) in 1 blocks (5.56%), avg size 20 bytes
  │   │         Reads:     0 bytes (0%, 0/Minstr), 0/byte
  │   │         Writes:    0 bytes (0%, 0/Minstr), 0/byte
  │   │         Accesses: {
  │   │           [  0]  - - - - - - - - - - - - - - - - - - - - 
  │   │         }
  │   │         Allocated at {
  │   │           ^1: 0x1087A5: g (big.c:18)
  │   │           ^2: 0x1087BF: h (big.c:18)
  │   │           ^3: 0x1087D9: i (big.c:18)
  │   │           ^4: 0x108841: j3 (big.c:20)
  │   │           ^5: 0x10885B: m (big.c:20)
  │   │           #6: 0x10888F: n2 (big.c:22)
  │   │           #7: 0x1088A9: o (big.c:22)
  │   │           #8: 0x108A99: main (big.c:47)
  │   │         }
  │   │       }
  │   └── AP 1.2.3/3 {
  │         Total:     10 bytes (1%, 39.47/Minstr)
  │         Allocated at {
  │           [2 insignificant]
  │         }
  │       }
  ├── AP 1.3/7 {
  │     Total:     30 bytes (3%, 118.41/Minstr) in 1 blocks (5.26%, 3.95/Minstr), avg size 30 bytes, avg lifetime 7,299 instrs (2.88% of program duration)
  │     Max:       30 bytes in 1 blocks, avg size 30 bytes
  │     At t-gmax: 0 bytes (0%) in 0 blocks (0%), avg size 0 bytes
  │     At t-end:  30 bytes (10.2%) in 1 blocks (5.56%), avg size 30 bytes
  │     Reads:     0 bytes (0%, 0/Minstr), 0/byte
  │     Writes:    0 bytes (0%, 0/Minstr), 0/byte
  │     Accesses: {
  │       [  0]  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  │     }
  │     Allocated at {
  │       #1: 0x108911: s1 (big.c:25)
  │       #2: 0x10892B: s2 (big.c:25)
  │       #3: 0x108945: s3 (big.c:25)
  │       #4: 0x10895F: s4 (big.c:25)
  │       #5: 0x108979: s5 (big.c:25)
  │       #6: 0x108AB7: main (big.c:50)
  │     }
  │   }
  ├─▼ AP 1.4/7 (1 children) {
  │     Total:     30 bytes (3%, 118.41/Minstr) in 5 blocks (26.32%, 19.74/Minstr), avg size 6 bytes, avg lifetime 7,056 instrs (2.79% of program duration)
  │     At t-gmax: 0 bytes (0%) in 0 blocks (0%), avg size 0 bytes
  │     At t-end:  30 bytes (10.2%) in 5 blocks (27.78%), avg size 6 bytes
  │     Reads:     0 bytes (0%, 0/Minstr), 0/byte
  │     Writes:    0 bytes (0%, 0/Minstr), 0/byte
  │     Allocated at {
  │       #1: 0x1089C7: v (big.c:28)
  │     }
  │   }
  │   └── AP 1.4.1/1 {
  │         Total:     30 bytes (3%, 118.41/Minstr)
  │         Allocated at {
  │           [4 insignificant]
  │         }
  │       }
  ├── AP 1.5/7 {
  │     Total:     20 bytes (2%, 78.94/Minstr) in 1 blocks (5.26%, 3.95/Minstr), avg size 20 bytes, avg lifetime 7,249 instrs (2.86% of program duration)
  │     Max:       20 bytes in 1 blocks, avg size 20 bytes
  │     At t-gmax: 0 bytes (0%) in 0 blocks (0%), avg size 0 bytes
  │     At t-end:  20 bytes (6.8%) in 1 blocks (5.56%), avg size 20 bytes
  │     Reads:     0 bytes (0%, 0/Minstr), 0/byte
  │     Writes:    0 bytes (0%, 0/Minstr), 0/byte
  │     Accesses: {
  │       [  0]  - - - - - - - - - - - - - - - - - - - - 
  │     }
  │     Allocated at {
  │       #1: 0x108993: t (big.c:26)
  │       #2: 0x108AC1: main (big.c:51)
  │     }
  │   }
  ├── AP 1.6/7 {
  │     Total:     19 bytes (1.9%, 74.99/Minstr) in 1 blocks (5.26%, 3.95/Minstr), avg size 19 bytes, avg lifetime 7,207 instrs (2.84% of program duration)
  │     Max:       19 bytes in 1 blocks, avg size 19 bytes
  │     At t-gmax: 0 bytes (0%) in 0 blocks (0%), avg size 0 bytes
  │     At t-end:  19 bytes (6.46%) in 1 blocks (5.56%), avg size 19 bytes
  │     Reads:     0 bytes (0%, 0/Minstr), 0/byte
  │     Writes:    0 bytes (0%, 0/Minstr), 0/byte
  │     Accesses: {
  │       [  0]  - - - - - - - - - - - - - - - - - - - 
  │     }
  │     Allocated at {
  │       #1: 0x1089AD: u (big.c:27)
  │       #2: 0x108ACB: main (big.c:52)
  │     }
  │   }
  └── AP 1.7/7 {
        Total:     10 bytes (1%, 39.47/Minstr) in 1 blocks (5.26%, 3.95/Minstr), avg size 10 bytes, avg lifetime 6,917 instrs (2.73% of program duration)
        Max:       10 bytes in 1 blocks, avg size 10 bytes
        At t-gmax: 0 bytes (0%) in 0 blocks (0%), avg size 0 bytes
        At t-end:  10 bytes (3.4%) in 1 blocks (5.56%), avg size 10 bytes
        Reads:     0 bytes (0%, 0/Minstr), 0/byte
        Writes:    0 bytes (0%, 0/Minstr), 0/byte
        Accesses: {
          [  0]  - - - - - - - - - - 
        }
        Allocated at {
          #1: 0x108B07: main (big.c:60)
        }
      }

AP significance threshold: total >= 10 bytes (1%)
`
//---------------------------------------------------------------------------
    },
    {
      label: "Total (blocks), short-lived",
      expected:
//---------------------------------------------------------------------------
`\
Invocation {
  Command: ./big
  PID:     3902
}

Times {
  t-gmax: 245,281 instrs (96.81% of program duration)
  t-end:  253,354 instrs
}

▼ AP 1/1 (1 children) {
    Total:     19 blocks (100%, 74.99/Minstr), avg lifetime 7,037.16 instrs (2.78% of program duration)
    Allocated at {
      #0: [root]
    }
  }
  └── AP 1.1/1 {
        Total:     19 blocks (100%, 74.99/Minstr), avg lifetime 7,037.16 instrs (2.78% of program duration)
        Allocated at {
          [7 insignificant]
        }
      }

AP significance threshold: (total >= 0.1 blocks (0.5%)) && (total avg lifetime <= 500 instrs)
`
//---------------------------------------------------------------------------
    },
    {
      label: "At t-gmax (bytes)",
      expected:
//---------------------------------------------------------------------------
`\
Invocation {
  Command: ./big
  PID:     3902
}

Times {
  t-gmax: 245,281 instrs (96.81% of program duration)
  t-end:  253,354 instrs
}

▼ AP 1/1 (2 children) {
    Total:     1,000 bytes (100%, 3,947.05/Minstr) in 19 blocks (100%, 74.99/Minstr), avg size 52.63 bytes, avg lifetime 7,037.16 instrs (2.78% of program duration)
    At t-gmax: 706 bytes (100%) in 1 blocks (100%), avg size 706 bytes
    At t-end:  294 bytes (100%) in 18 blocks (100%), avg size 16.33 bytes
    Reads:     0 bytes (0%, 0/Minstr), 0/byte
    Writes:    0 bytes (0%, 0/Minstr), 0/byte
    Allocated at {
      #0: [root]
    }
  }
  ├─▼ AP 1.1/2 (2 children) {
  │     Total:     771 bytes (77.1%, 3,043.17/Minstr) in 5 blocks (26.32%, 19.74/Minstr), avg size 154.2 bytes, avg lifetime 6,414.8 instrs (2.53% of program duration)
  │     At t-gmax: 706 bytes (100%) in 1 blocks (100%), avg size 706 bytes
  │     At t-end:  65 bytes (22.11%) in 4 blocks (22.22%), avg size 16.25 bytes
  │     Reads:     0 bytes (0%, 0/Minstr), 0/byte
  │     Writes:    0 bytes (0%, 0/Minstr), 0/byte
  │     Allocated at {
  │       #1: 0x1086A1: a (big.c:10)
  │     }
  │   }
  │   ├─▼ AP 1.1.1/2 (2 children) {
  │   │     Total:     741 bytes (74.1%, 2,924.76/Minstr) in 3 blocks (15.79%, 11.84/Minstr), avg size 247 bytes, avg lifetime 5,475 instrs (2.16% of program duration)
  │   │     At t-gmax: 706 bytes (100%) in 1 blocks (100%), avg size 706 bytes
  │   │     At t-end:  35 bytes (11.9%) in 2 blocks (11.11%), avg size 17.5 bytes
  │   │     Reads:     0 bytes (0%, 0/Minstr), 0/byte
  │   │     Writes:    0 bytes (0%, 0/Minstr), 0/byte
  │   │     Allocated at {
  │   │       ^1: 0x1086A1: a (big.c:10)
  │   │       #2: 0x1086BB: b1 (big.c:11)
  │   │     }
  │   │   }
  │   │   ├─▼ AP 1.1.1.1/2 (2 children) {
  │   │   │     Total:     711 bytes (71.1%, 2,806.35/Minstr) in 2 blocks (10.53%, 7.89/Minstr), avg size 355.5 bytes, avg lifetime 4,257.5 instrs (1.68% of program duration)
  │   │   │     At t-gmax: 706 bytes (100%) in 1 blocks (100%), avg size 706 bytes
  │   │   │     At t-end:  5 bytes (1.7%) in 1 blocks (5.56%), avg size 5 bytes
  │   │   │     Reads:     0 bytes (0%, 0/Minstr), 0/byte
  │   │   │     Writes:    0 bytes (0%, 0/Minstr), 0/byte
  │   │   │     Allocated at {
  │   │   │       ^1: 0x1086A1: a (big.c:10)
  │   │   │       ^2: 0x1086BB: b1 (big.c:11)
  │   │   │       #3: 0x1086D5: c1 (big.c:12)
  │   │   │     }
  │   │   │   }
  │   │   │   ├── AP 1.1.1.1.1/2 {
  │   │   │   │     Total:     706 bytes (70.6%, 2,786.61/Minstr) in 1 blocks (5.26%, 3.95/Minstr), avg size 706 bytes, avg lifetime 543 instrs (0.21% of program duration)
  │   │   │   │     Max:       706 bytes in 1 blocks, avg size 706 bytes
  │   │   │   │     At t-gmax: 706 bytes (100%) in 1 blocks (100%), avg size 706 bytes
  │   │   │   │     At t-end:  0 bytes (0%) in 0 blocks (0%), avg size 0 bytes
  │   │   │   │     Reads:     0 bytes (0%, 0/Minstr), 0/byte
  │   │   │   │     Writes:    0 bytes (0%, 0/Minstr), 0/byte
  │   │   │   │     Accesses: {
  │   │   │   │       [  0]  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  │   │   │   │       [ 32]  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  │   │   │   │       [ 64]  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  │   │   │   │       [ 96]  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  │   │   │   │       [128]  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  │   │   │   │       [160]  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  │   │   │   │       [192]  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  │   │   │   │       [224]  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  │   │   │   │       [256]  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  │   │   │   │       [288]  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  │   │   │   │       [320]  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  │   │   │   │       [352]  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  │   │   │   │       [384]  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  │   │   │   │       [416]  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  │   │   │   │       [448]  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  │   │   │   │       [480]  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  │   │   │   │       [512]  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  │   │   │   │       [544]  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  │   │   │   │       [576]  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  │   │   │   │       [608]  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  │   │   │   │       [640]  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  │   │   │   │       [672]  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  │   │   │   │       [704]  - - 
  │   │   │   │     }
  │   │   │   │     Allocated at {
  │   │   │   │       ^1: 0x1086A1: a (big.c:10)
  │   │   │   │       ^2: 0x1086BB: b1 (big.c:11)
  │   │   │   │       ^3: 0x1086D5: c1 (big.c:12)
  │   │   │   │       #4: 0x1086EF: d1 (big.c:13)
  │   │   │   │       #5: 0x108A43: main (big.c:38)
  │   │   │   │     }
  │   │   │   │   }
  │   │   │   └── AP 1.1.1.1.2/2 {
  │   │   │         At t-gmax: 0 bytes (0%)
  │   │   │         Allocated at {
  │   │   │           [1 insignificant]
  │   │   │         }
  │   │   │       }
  │   │   └── AP 1.1.1.2/2 {
  │   │         At t-gmax: 0 bytes (0%)
  │   │         Allocated at {
  │   │           [1 insignificant]
  │   │         }
  │   │       }
  │   └── AP 1.1.2/2 {
  │         At t-gmax: 0 bytes (0%)
  │         Allocated at {
  │           [2 insignificant]
  │         }
  │       }
  └── AP 1.2/2 {
        At t-gmax: 0 bytes (0%)
        Allocated at {
          [6 insignificant]
        }
      }

AP significance threshold: at-t-gmax >= 7.06 bytes (1%)
`
//---------------------------------------------------------------------------
    }
  ]
};
tests.push(big);

//---------------------------------------------------------------------------
// sig (corresponds to dhat/tests/sig.c)
//---------------------------------------------------------------------------

let sig = {
  name: "sig",
  input:
//---------------------------------------------------------------------------
{"dhatFileVersion":1
,"cmd":"./sig"
,"pid":21476
,"mi":1311861,"ei":1318783
,"aps":
 [{"tb":11,"tbk":1,"tli":1075941
  ,"mb":11,"mbk":1
  ,"gb":11,"gbk":1
  ,"fb":11,"fbk":1
  ,"rb":11,"wb":16489
  ,"acc":[-11,1500]
  ,"fs":[1,2]
  }
 ,{"tb":10,"tbk":1,"tli":880845
  ,"mb":10,"mbk":1
  ,"gb":10,"gbk":1
  ,"fb":10,"fbk":1
  ,"rb":10,"wb":14990
  ,"acc":[-10,1500]
  ,"fs":[1,3,4]
  }
 ,{"tb":5,"tbk":1,"tli":702250
  ,"mb":5,"mbk":1
  ,"gb":5,"gbk":1
  ,"fb":5,"fbk":1
  ,"rb":5,"wb":7495
  ,"acc":[-5,1500]
  ,"fs":[1,5,6]
  }
 ,{"tb":4,"tbk":1,"tli":606170
  ,"mb":4,"mbk":1
  ,"gb":4,"gbk":1
  ,"fb":4,"fbk":1
  ,"rb":4,"wb":5996
  ,"acc":[-4,1500]
  ,"fs":[1,5,7]
  }
 ,{"tb":10,"tbk":1,"tli":510097
  ,"mb":10,"mbk":1
  ,"gb":10,"gbk":1
  ,"fb":10,"fbk":1
  ,"rb":10,"wb":14990
  ,"acc":[-10,1500]
  ,"fs":[8,9]
  }
 ,{"tb":9,"tbk":1,"tli":331504
  ,"mb":9,"mbk":1
  ,"gb":9,"gbk":1
  ,"fb":9,"fbk":1
  ,"rb":9,"wb":13491
  ,"acc":[-9,1500]
  ,"fs":[8,10,11]
  }
 ,{"tb":5,"tbk":1,"tli":169412
  ,"mb":5,"mbk":1
  ,"gb":5,"gbk":1
  ,"fb":5,"fbk":1
  ,"rb":0,"wb":0
  ,"acc":[-5,0]
  ,"fs":[8,12,13]
  }
 ,{"tb":3,"tbk":1,"tli":169360
  ,"mb":3,"mbk":1
  ,"gb":3,"gbk":1
  ,"fb":3,"fbk":1
  ,"rb":0,"wb":0
  ,"acc":[-3,0]
  ,"fs":[8,12,14]
  }
 ,{"tb":9,"tbk":1,"tli":169315
  ,"mb":9,"mbk":1
  ,"gb":9,"gbk":1
  ,"fb":9,"fbk":1
  ,"rb":9,"wb":13491
  ,"acc":[-9,1500]
  ,"fs":[15,16]
  }
 ,{"tb":8,"tbk":1,"tli":7225
  ,"mb":8,"mbk":1
  ,"gb":8,"gbk":1
  ,"fb":8,"fbk":1
  ,"rb":0,"wb":0
  ,"acc":[-8,0]
  ,"fs":[15,17,18]
  }
 ,{"tb":4,"tbk":1,"tli":7173
  ,"mb":4,"mbk":1
  ,"gb":4,"gbk":1
  ,"fb":4,"fbk":1
  ,"rb":0,"wb":0
  ,"acc":[-4,0]
  ,"fs":[15,19,20]
  }
 ,{"tb":3,"tbk":1,"tli":7121
  ,"mb":3,"mbk":1
  ,"gb":3,"gbk":1
  ,"fb":3,"fbk":1
  ,"rb":0,"wb":0
  ,"acc":[-3,0]
  ,"fs":[15,19,21]
  }
 ,{"tb":8,"tbk":1,"tli":7076
  ,"mb":8,"mbk":1
  ,"gb":8,"gbk":1
  ,"fb":8,"fbk":1
  ,"rb":0,"wb":0
  ,"acc":[-8,0]
  ,"fs":[22,23]
  }
 ,{"tb":7,"tbk":1,"tli":7026
  ,"mb":7,"mbk":1
  ,"gb":7,"gbk":1
  ,"fb":7,"fbk":1
  ,"rb":0,"wb":0
  ,"acc":[-7,0]
  ,"fs":[22,24,25]
  }
 ,{"tb":4,"tbk":1,"tli":6974
  ,"mb":4,"mbk":1
  ,"gb":4,"gbk":1
  ,"fb":4,"fbk":1
  ,"rb":0,"wb":0
  ,"acc":[-4,0]
  ,"fs":[22,26,27]
  }
 ,{"tb":2,"tbk":1,"tli":6922
  ,"mb":2,"mbk":1
  ,"gb":2,"gbk":1
  ,"fb":2,"fbk":1
  ,"rb":0,"wb":0
  ,"acc":[-2,0]
  ,"fs":[22,26,28]
  }
 ]
,"ftbl":
 ["[root]"
 ,"0x108681: am (sig.c:9)"
 ,"0x10883C: main (sig.c:57)"
 ,"0x10869B: a2 (sig.c:11)"
 ,"0x10885B: main (sig.c:58)"
 ,"0x1086B5: a3 (sig.c:12)"
 ,"0x10887A: main (sig.c:59)"
 ,"0x108899: main (sig.c:60)"
 ,"0x1086CF: bm (sig.c:15)"
 ,"0x1088B8: main (sig.c:62)"
 ,"0x1086E9: b2 (sig.c:17)"
 ,"0x1088D7: main (sig.c:63)"
 ,"0x108703: b3 (sig.c:18)"
 ,"0x1088F6: main (sig.c:64)"
 ,"0x108904: main (sig.c:65)"
 ,"0x10871D: cm (sig.c:21)"
 ,"0x108912: main (sig.c:67)"
 ,"0x108737: c2 (sig.c:23)"
 ,"0x108931: main (sig.c:68)"
 ,"0x108751: c3 (sig.c:24)"
 ,"0x10893F: main (sig.c:69)"
 ,"0x10894D: main (sig.c:70)"
 ,"0x10876B: dm (sig.c:27)"
 ,"0x10895B: main (sig.c:72)"
 ,"0x108785: d2 (sig.c:29)"
 ,"0x108969: main (sig.c:73)"
 ,"0x10879F: d3 (sig.c:30)"
 ,"0x108977: main (sig.c:74)"
 ,"0x108985: main (sig.c:75)"
 ]
}
//---------------------------------------------------------------------------
  ,
  outputs: [
    {
      label: "Total (bytes), zero reads or zero writes",
      expected:
//---------------------------------------------------------------------------
`\
Invocation {
  Command: ./sig
  PID:     21476
}

Times {
  t-gmax: 1,311,861 instrs (99.48% of program duration)
  t-end:  1,318,783 instrs
}

▼ AP 1/1 (4 children) {
    Total:     102 bytes (100%, 77.34/Minstr)
    Reads:     58 bytes (100%, 43.98/Minstr)
    Writes:    86,942 bytes (100%, 65,925.93/Minstr)
    Allocated at {
      #0: [root]
    }
  }
  ├── AP 1.1/4 {
  │     Total:     30 bytes (29.41%, 22.75/Minstr)
  │     Reads:     30 bytes (51.72%, 22.75/Minstr)
  │     Writes:    44,970 bytes (51.72%, 34,099.62/Minstr)
  │     Allocated at {
  │       [1 insignificant]
  │     }
  │   }
  ├─▼ AP 1.2/4 (2 children) {
  │     Total:     27 bytes (26.47%, 20.47/Minstr)
  │     Reads:     19 bytes (32.76%, 14.41/Minstr)
  │     Writes:    28,481 bytes (32.76%, 21,596.43/Minstr)
  │     Allocated at {
  │       #1: 0x1086CF: bm (sig.c:15)
  │     }
  │   }
  │   ├── AP 1.2.1/2 {
  │   │     Total:     19 bytes (18.63%, 14.41/Minstr)
  │   │     Reads:     19 bytes (32.76%, 14.41/Minstr)
  │   │     Writes:    28,481 bytes (32.76%, 21,596.43/Minstr)
  │   │     Allocated at {
  │   │       [2 insignificant]
  │   │     }
  │   │   }
  │   └─▼ AP 1.2.2/2 (2 children) {
  │         Total:     8 bytes (7.84%, 6.07/Minstr) in 2 blocks (12.5%, 1.52/Minstr), avg size 4 bytes, avg lifetime 169,386 instrs (12.84% of program duration)
  │         At t-gmax: 8 bytes (7.84%) in 2 blocks (12.5%), avg size 4 bytes
  │         At t-end:  8 bytes (7.84%) in 2 blocks (12.5%), avg size 4 bytes
  │         Reads:     0 bytes (0%, 0/Minstr), 0/byte
  │         Writes:    0 bytes (0%, 0/Minstr), 0/byte
  │         Allocated at {
  │           ^1: 0x1086CF: bm (sig.c:15)
  │           #2: 0x108703: b3 (sig.c:18)
  │         }
  │       }
  │       ├── AP 1.2.2.1/2 {
  │       │     Total:     5 bytes (4.9%, 3.79/Minstr) in 1 blocks (6.25%, 0.76/Minstr), avg size 5 bytes, avg lifetime 169,412 instrs (12.85% of program duration)
  │       │     Max:       5 bytes in 1 blocks, avg size 5 bytes
  │       │     At t-gmax: 5 bytes (4.9%) in 1 blocks (6.25%), avg size 5 bytes
  │       │     At t-end:  5 bytes (4.9%) in 1 blocks (6.25%), avg size 5 bytes
  │       │     Reads:     0 bytes (0%, 0/Minstr), 0/byte
  │       │     Writes:    0 bytes (0%, 0/Minstr), 0/byte
  │       │     Accesses: {
  │       │       [  0]  - - - - - 
  │       │     }
  │       │     Allocated at {
  │       │       ^1: 0x1086CF: bm (sig.c:15)
  │       │       ^2: 0x108703: b3 (sig.c:18)
  │       │       #3: 0x1088F6: main (sig.c:64)
  │       │     }
  │       │   }
  │       └── AP 1.2.2.2/2 {
  │             Total:     3 bytes (2.94%, 2.27/Minstr) in 1 blocks (6.25%, 0.76/Minstr), avg size 3 bytes, avg lifetime 169,360 instrs (12.84% of program duration)
  │             Max:       3 bytes in 1 blocks, avg size 3 bytes
  │             At t-gmax: 3 bytes (2.94%) in 1 blocks (6.25%), avg size 3 bytes
  │             At t-end:  3 bytes (2.94%) in 1 blocks (6.25%), avg size 3 bytes
  │             Reads:     0 bytes (0%, 0/Minstr), 0/byte
  │             Writes:    0 bytes (0%, 0/Minstr), 0/byte
  │             Accesses: {
  │               [  0]  - - - 
  │             }
  │             Allocated at {
  │               ^1: 0x1086CF: bm (sig.c:15)
  │               ^2: 0x108703: b3 (sig.c:18)
  │               #3: 0x108904: main (sig.c:65)
  │             }
  │           }
  ├─▼ AP 1.3/4 (3 children) {
  │     Total:     24 bytes (23.53%, 18.2/Minstr)
  │     Reads:     9 bytes (15.52%, 6.82/Minstr)
  │     Writes:    13,491 bytes (15.52%, 10,229.89/Minstr)
  │     Allocated at {
  │       #1: 0x10871D: cm (sig.c:21)
  │     }
  │   }
  │   ├── AP 1.3.1/3 {
  │   │     Total:     9 bytes (8.82%, 6.82/Minstr)
  │   │     Reads:     9 bytes (15.52%, 6.82/Minstr)
  │   │     Writes:    13,491 bytes (15.52%, 10,229.89/Minstr)
  │   │     Allocated at {
  │   │       [1 insignificant]
  │   │     }
  │   │   }
  │   ├── AP 1.3.2/3 {
  │   │     Total:     8 bytes (7.84%, 6.07/Minstr) in 1 blocks (6.25%, 0.76/Minstr), avg size 8 bytes, avg lifetime 7,225 instrs (0.55% of program duration)
  │   │     Max:       8 bytes in 1 blocks, avg size 8 bytes
  │   │     At t-gmax: 8 bytes (7.84%) in 1 blocks (6.25%), avg size 8 bytes
  │   │     At t-end:  8 bytes (7.84%) in 1 blocks (6.25%), avg size 8 bytes
  │   │     Reads:     0 bytes (0%, 0/Minstr), 0/byte
  │   │     Writes:    0 bytes (0%, 0/Minstr), 0/byte
  │   │     Accesses: {
  │   │       [  0]  - - - - - - - - 
  │   │     }
  │   │     Allocated at {
  │   │       ^1: 0x10871D: cm (sig.c:21)
  │   │       #2: 0x108737: c2 (sig.c:23)
  │   │       #3: 0x108931: main (sig.c:68)
  │   │     }
  │   │   }
  │   └─▼ AP 1.3.3/3 (2 children) {
  │         Total:     7 bytes (6.86%, 5.31/Minstr) in 2 blocks (12.5%, 1.52/Minstr), avg size 3.5 bytes, avg lifetime 7,147 instrs (0.54% of program duration)
  │         At t-gmax: 7 bytes (6.86%) in 2 blocks (12.5%), avg size 3.5 bytes
  │         At t-end:  7 bytes (6.86%) in 2 blocks (12.5%), avg size 3.5 bytes
  │         Reads:     0 bytes (0%, 0/Minstr), 0/byte
  │         Writes:    0 bytes (0%, 0/Minstr), 0/byte
  │         Allocated at {
  │           ^1: 0x10871D: cm (sig.c:21)
  │           #2: 0x108751: c3 (sig.c:24)
  │         }
  │       }
  │       ├── AP 1.3.3.1/2 {
  │       │     Total:     4 bytes (3.92%, 3.03/Minstr) in 1 blocks (6.25%, 0.76/Minstr), avg size 4 bytes, avg lifetime 7,173 instrs (0.54% of program duration)
  │       │     Max:       4 bytes in 1 blocks, avg size 4 bytes
  │       │     At t-gmax: 4 bytes (3.92%) in 1 blocks (6.25%), avg size 4 bytes
  │       │     At t-end:  4 bytes (3.92%) in 1 blocks (6.25%), avg size 4 bytes
  │       │     Reads:     0 bytes (0%, 0/Minstr), 0/byte
  │       │     Writes:    0 bytes (0%, 0/Minstr), 0/byte
  │       │     Accesses: {
  │       │       [  0]  - - - - 
  │       │     }
  │       │     Allocated at {
  │       │       ^1: 0x10871D: cm (sig.c:21)
  │       │       ^2: 0x108751: c3 (sig.c:24)
  │       │       #3: 0x10893F: main (sig.c:69)
  │       │     }
  │       │   }
  │       └── AP 1.3.3.2/2 {
  │             Total:     3 bytes (2.94%, 2.27/Minstr) in 1 blocks (6.25%, 0.76/Minstr), avg size 3 bytes, avg lifetime 7,121 instrs (0.54% of program duration)
  │             Max:       3 bytes in 1 blocks, avg size 3 bytes
  │             At t-gmax: 3 bytes (2.94%) in 1 blocks (6.25%), avg size 3 bytes
  │             At t-end:  3 bytes (2.94%) in 1 blocks (6.25%), avg size 3 bytes
  │             Reads:     0 bytes (0%, 0/Minstr), 0/byte
  │             Writes:    0 bytes (0%, 0/Minstr), 0/byte
  │             Accesses: {
  │               [  0]  - - - 
  │             }
  │             Allocated at {
  │               ^1: 0x10871D: cm (sig.c:21)
  │               ^2: 0x108751: c3 (sig.c:24)
  │               #3: 0x10894D: main (sig.c:70)
  │             }
  │           }
  └─▼ AP 1.4/4 (3 children) {
        Total:     21 bytes (20.59%, 15.92/Minstr) in 4 blocks (25%, 3.03/Minstr), avg size 5.25 bytes, avg lifetime 6,999.5 instrs (0.53% of program duration)
        At t-gmax: 21 bytes (20.59%) in 4 blocks (25%), avg size 5.25 bytes
        At t-end:  21 bytes (20.59%) in 4 blocks (25%), avg size 5.25 bytes
        Reads:     0 bytes (0%, 0/Minstr), 0/byte
        Writes:    0 bytes (0%, 0/Minstr), 0/byte
        Allocated at {
          #1: 0x10876B: dm (sig.c:27)
        }
      }
      ├── AP 1.4.1/3 {
      │     Total:     8 bytes (7.84%, 6.07/Minstr) in 1 blocks (6.25%, 0.76/Minstr), avg size 8 bytes, avg lifetime 7,076 instrs (0.54% of program duration)
      │     Max:       8 bytes in 1 blocks, avg size 8 bytes
      │     At t-gmax: 8 bytes (7.84%) in 1 blocks (6.25%), avg size 8 bytes
      │     At t-end:  8 bytes (7.84%) in 1 blocks (6.25%), avg size 8 bytes
      │     Reads:     0 bytes (0%, 0/Minstr), 0/byte
      │     Writes:    0 bytes (0%, 0/Minstr), 0/byte
      │     Accesses: {
      │       [  0]  - - - - - - - - 
      │     }
      │     Allocated at {
      │       ^1: 0x10876B: dm (sig.c:27)
      │       #2: 0x10895B: main (sig.c:72)
      │     }
      │   }
      ├── AP 1.4.2/3 {
      │     Total:     7 bytes (6.86%, 5.31/Minstr) in 1 blocks (6.25%, 0.76/Minstr), avg size 7 bytes, avg lifetime 7,026 instrs (0.53% of program duration)
      │     Max:       7 bytes in 1 blocks, avg size 7 bytes
      │     At t-gmax: 7 bytes (6.86%) in 1 blocks (6.25%), avg size 7 bytes
      │     At t-end:  7 bytes (6.86%) in 1 blocks (6.25%), avg size 7 bytes
      │     Reads:     0 bytes (0%, 0/Minstr), 0/byte
      │     Writes:    0 bytes (0%, 0/Minstr), 0/byte
      │     Accesses: {
      │       [  0]  - - - - - - - 
      │     }
      │     Allocated at {
      │       ^1: 0x10876B: dm (sig.c:27)
      │       #2: 0x108785: d2 (sig.c:29)
      │       #3: 0x108969: main (sig.c:73)
      │     }
      │   }
      └─▼ AP 1.4.3/3 (2 children) {
            Total:     6 bytes (5.88%, 4.55/Minstr) in 2 blocks (12.5%, 1.52/Minstr), avg size 3 bytes, avg lifetime 6,948 instrs (0.53% of program duration)
            At t-gmax: 6 bytes (5.88%) in 2 blocks (12.5%), avg size 3 bytes
            At t-end:  6 bytes (5.88%) in 2 blocks (12.5%), avg size 3 bytes
            Reads:     0 bytes (0%, 0/Minstr), 0/byte
            Writes:    0 bytes (0%, 0/Minstr), 0/byte
            Allocated at {
              ^1: 0x10876B: dm (sig.c:27)
              #2: 0x10879F: d3 (sig.c:30)
            }
          }
          ├── AP 1.4.3.1/2 {
          │     Total:     4 bytes (3.92%, 3.03/Minstr) in 1 blocks (6.25%, 0.76/Minstr), avg size 4 bytes, avg lifetime 6,974 instrs (0.53% of program duration)
          │     Max:       4 bytes in 1 blocks, avg size 4 bytes
          │     At t-gmax: 4 bytes (3.92%) in 1 blocks (6.25%), avg size 4 bytes
          │     At t-end:  4 bytes (3.92%) in 1 blocks (6.25%), avg size 4 bytes
          │     Reads:     0 bytes (0%, 0/Minstr), 0/byte
          │     Writes:    0 bytes (0%, 0/Minstr), 0/byte
          │     Accesses: {
          │       [  0]  - - - - 
          │     }
          │     Allocated at {
          │       ^1: 0x10876B: dm (sig.c:27)
          │       ^2: 0x10879F: d3 (sig.c:30)
          │       #3: 0x108977: main (sig.c:74)
          │     }
          │   }
          └── AP 1.4.3.2/2 {
                Total:     2 bytes (1.96%, 1.52/Minstr) in 1 blocks (6.25%, 0.76/Minstr), avg size 2 bytes, avg lifetime 6,922 instrs (0.52% of program duration)
                Max:       2 bytes in 1 blocks, avg size 2 bytes
                At t-gmax: 2 bytes (1.96%) in 1 blocks (6.25%), avg size 2 bytes
                At t-end:  2 bytes (1.96%) in 1 blocks (6.25%), avg size 2 bytes
                Reads:     0 bytes (0%, 0/Minstr), 0/byte
                Writes:    0 bytes (0%, 0/Minstr), 0/byte
                Accesses: {
                  [  0]  - - 
                }
                Allocated at {
                  ^1: 0x10876B: dm (sig.c:27)
                  ^2: 0x10879F: d3 (sig.c:30)
                  #3: 0x108985: main (sig.c:75)
                }
              }

AP significance threshold: (total >= 0.51 bytes (0.5%)) && ((reads == 0 bytes) || (writes == 0 bytes))
`
//---------------------------------------------------------------------------
    },
    {
      label: "Total (blocks), low-access",
      expected:
//---------------------------------------------------------------------------
`\
Invocation {
  Command: ./sig
  PID:     21476
}

Times {
  t-gmax: 1,311,861 instrs (99.48% of program duration)
  t-end:  1,318,783 instrs
}

▼ AP 1/1 (2 children) {
    Total:     16 blocks (100%, 12.13/Minstr)
    Reads:     0.57/byte
    Writes:    852.37/byte
    Allocated at {
      #0: [root]
    }
  }
  ├── AP 1.1/2 {
  │     Total:     12 blocks (75%, 9.1/Minstr)
  │     Reads:     0.63/byte
  │     Writes:    941.68/byte
  │     Allocated at {
  │       [3 insignificant]
  │     }
  │   }
  └─▼ AP 1.2/2 (1 children) {
        Total:     24 bytes (23.53%, 18.2/Minstr) in 4 blocks (25%, 3.03/Minstr), avg size 6 bytes, avg lifetime 47,708.5 instrs (3.62% of program duration)
        At t-gmax: 24 bytes (23.53%) in 4 blocks (25%), avg size 6 bytes
        At t-end:  24 bytes (23.53%) in 4 blocks (25%), avg size 6 bytes
        Reads:     9 bytes (15.52%, 6.82/Minstr), 0.38/byte
        Writes:    13,491 bytes (15.52%, 10,229.89/Minstr), 562.13/byte
        Allocated at {
          #1: 0x10871D: cm (sig.c:21)
        }
      }
      └── AP 1.2.1/1 {
            Total:     4 blocks (25%, 3.03/Minstr)
            Reads:     0.38/byte
            Writes:    562.13/byte
            Allocated at {
              [3 insignificant]
            }
          }

AP significance threshold: (total >= 0.08 blocks (0.5%)) && (reads != 0 bytes) && (writes != 0 bytes) && ((reads <= 0.4/byte) || (writes <= 0.4/byte))
`
//---------------------------------------------------------------------------
    },
    {
      label: "Writes (bytes), high-access",
      expected:
//---------------------------------------------------------------------------
`\
Invocation {
  Command: ./sig
  PID:     21476
}

Times {
  t-gmax: 1,311,861 instrs (99.48% of program duration)
  t-end:  1,318,783 instrs
}

▼ AP 1/1 (4 children) {
    Writes:    86,942 bytes (100%, 65,925.93/Minstr), 852.37/byte
    Allocated at {
      #0: [root]
    }
  }
  ├─▼ AP 1.1/4 (3 children) {
  │     Total:     30 bytes (29.41%, 22.75/Minstr) in 4 blocks (25%, 3.03/Minstr), avg size 7.5 bytes, avg lifetime 816,301.5 instrs (61.9% of program duration)
  │     At t-gmax: 30 bytes (29.41%) in 4 blocks (25%), avg size 7.5 bytes
  │     At t-end:  30 bytes (29.41%) in 4 blocks (25%), avg size 7.5 bytes
  │     Reads:     30 bytes (51.72%, 22.75/Minstr), 1/byte
  │     Writes:    44,970 bytes (51.72%, 34,099.62/Minstr), 1,499/byte
  │     Allocated at {
  │       #1: 0x108681: am (sig.c:9)
  │     }
  │   }
  │   ├── AP 1.1.1/3 {
  │   │     Total:     11 bytes (10.78%, 8.34/Minstr) in 1 blocks (6.25%, 0.76/Minstr), avg size 11 bytes, avg lifetime 1,075,941 instrs (81.59% of program duration)
  │   │     Max:       11 bytes in 1 blocks, avg size 11 bytes
  │   │     At t-gmax: 11 bytes (10.78%) in 1 blocks (6.25%), avg size 11 bytes
  │   │     At t-end:  11 bytes (10.78%) in 1 blocks (6.25%), avg size 11 bytes
  │   │     Reads:     11 bytes (18.97%, 8.34/Minstr), 1/byte
  │   │     Writes:    16,489 bytes (18.97%, 12,503.19/Minstr), 1,499/byte
  │   │     Accesses: {
  │   │       [  0]  1500 〃 〃 〃 〃 〃 〃 〃 〃 〃 〃 
  │   │     }
  │   │     Allocated at {
  │   │       ^1: 0x108681: am (sig.c:9)
  │   │       #2: 0x10883C: main (sig.c:57)
  │   │     }
  │   │   }
  │   ├── AP 1.1.2/3 {
  │   │     Total:     10 bytes (9.8%, 7.58/Minstr) in 1 blocks (6.25%, 0.76/Minstr), avg size 10 bytes, avg lifetime 880,845 instrs (66.79% of program duration)
  │   │     Max:       10 bytes in 1 blocks, avg size 10 bytes
  │   │     At t-gmax: 10 bytes (9.8%) in 1 blocks (6.25%), avg size 10 bytes
  │   │     At t-end:  10 bytes (9.8%) in 1 blocks (6.25%), avg size 10 bytes
  │   │     Reads:     10 bytes (17.24%, 7.58/Minstr), 1/byte
  │   │     Writes:    14,990 bytes (17.24%, 11,366.54/Minstr), 1,499/byte
  │   │     Accesses: {
  │   │       [  0]  1500 〃 〃 〃 〃 〃 〃 〃 〃 〃 
  │   │     }
  │   │     Allocated at {
  │   │       ^1: 0x108681: am (sig.c:9)
  │   │       #2: 0x10869B: a2 (sig.c:11)
  │   │       #3: 0x10885B: main (sig.c:58)
  │   │     }
  │   │   }
  │   └─▼ AP 1.1.3/3 (2 children) {
  │         Total:     9 bytes (8.82%, 6.82/Minstr) in 2 blocks (12.5%, 1.52/Minstr), avg size 4.5 bytes, avg lifetime 654,210 instrs (49.61% of program duration)
  │         At t-gmax: 9 bytes (8.82%) in 2 blocks (12.5%), avg size 4.5 bytes
  │         At t-end:  9 bytes (8.82%) in 2 blocks (12.5%), avg size 4.5 bytes
  │         Reads:     9 bytes (15.52%, 6.82/Minstr), 1/byte
  │         Writes:    13,491 bytes (15.52%, 10,229.89/Minstr), 1,499/byte
  │         Allocated at {
  │           ^1: 0x108681: am (sig.c:9)
  │           #2: 0x1086B5: a3 (sig.c:12)
  │         }
  │       }
  │       ├── AP 1.1.3.1/2 {
  │       │     Total:     5 bytes (4.9%, 3.79/Minstr) in 1 blocks (6.25%, 0.76/Minstr), avg size 5 bytes, avg lifetime 702,250 instrs (53.25% of program duration)
  │       │     Max:       5 bytes in 1 blocks, avg size 5 bytes
  │       │     At t-gmax: 5 bytes (4.9%) in 1 blocks (6.25%), avg size 5 bytes
  │       │     At t-end:  5 bytes (4.9%) in 1 blocks (6.25%), avg size 5 bytes
  │       │     Reads:     5 bytes (8.62%, 3.79/Minstr), 1/byte
  │       │     Writes:    7,495 bytes (8.62%, 5,683.27/Minstr), 1,499/byte
  │       │     Accesses: {
  │       │       [  0]  1500 〃 〃 〃 〃 
  │       │     }
  │       │     Allocated at {
  │       │       ^1: 0x108681: am (sig.c:9)
  │       │       ^2: 0x1086B5: a3 (sig.c:12)
  │       │       #3: 0x10887A: main (sig.c:59)
  │       │     }
  │       │   }
  │       └── AP 1.1.3.2/2 {
  │             Total:     4 bytes (3.92%, 3.03/Minstr) in 1 blocks (6.25%, 0.76/Minstr), avg size 4 bytes, avg lifetime 606,170 instrs (45.96% of program duration)
  │             Max:       4 bytes in 1 blocks, avg size 4 bytes
  │             At t-gmax: 4 bytes (3.92%) in 1 blocks (6.25%), avg size 4 bytes
  │             At t-end:  4 bytes (3.92%) in 1 blocks (6.25%), avg size 4 bytes
  │             Reads:     4 bytes (6.9%, 3.03/Minstr), 1/byte
  │             Writes:    5,996 bytes (6.9%, 4,546.62/Minstr), 1,499/byte
  │             Accesses: {
  │               [  0]  1500 〃 〃 〃 
  │             }
  │             Allocated at {
  │               ^1: 0x108681: am (sig.c:9)
  │               ^2: 0x1086B5: a3 (sig.c:12)
  │               #3: 0x108899: main (sig.c:60)
  │             }
  │           }
  ├─▼ AP 1.2/4 (3 children) {
  │     Total:     27 bytes (26.47%, 20.47/Minstr) in 4 blocks (25%, 3.03/Minstr), avg size 6.75 bytes, avg lifetime 295,093.25 instrs (22.38% of program duration)
  │     At t-gmax: 27 bytes (26.47%) in 4 blocks (25%), avg size 6.75 bytes
  │     At t-end:  27 bytes (26.47%) in 4 blocks (25%), avg size 6.75 bytes
  │     Reads:     19 bytes (32.76%, 14.41/Minstr), 0.7/byte
  │     Writes:    28,481 bytes (32.76%, 21,596.43/Minstr), 1,054.85/byte
  │     Allocated at {
  │       #1: 0x1086CF: bm (sig.c:15)
  │     }
  │   }
  │   ├── AP 1.2.1/3 {
  │   │     Total:     10 bytes (9.8%, 7.58/Minstr) in 1 blocks (6.25%, 0.76/Minstr), avg size 10 bytes, avg lifetime 510,097 instrs (38.68% of program duration)
  │   │     Max:       10 bytes in 1 blocks, avg size 10 bytes
  │   │     At t-gmax: 10 bytes (9.8%) in 1 blocks (6.25%), avg size 10 bytes
  │   │     At t-end:  10 bytes (9.8%) in 1 blocks (6.25%), avg size 10 bytes
  │   │     Reads:     10 bytes (17.24%, 7.58/Minstr), 1/byte
  │   │     Writes:    14,990 bytes (17.24%, 11,366.54/Minstr), 1,499/byte
  │   │     Accesses: {
  │   │       [  0]  1500 〃 〃 〃 〃 〃 〃 〃 〃 〃 
  │   │     }
  │   │     Allocated at {
  │   │       ^1: 0x1086CF: bm (sig.c:15)
  │   │       #2: 0x1088B8: main (sig.c:62)
  │   │     }
  │   │   }
  │   ├── AP 1.2.2/3 {
  │   │     Total:     9 bytes (8.82%, 6.82/Minstr) in 1 blocks (6.25%, 0.76/Minstr), avg size 9 bytes, avg lifetime 331,504 instrs (25.14% of program duration)
  │   │     Max:       9 bytes in 1 blocks, avg size 9 bytes
  │   │     At t-gmax: 9 bytes (8.82%) in 1 blocks (6.25%), avg size 9 bytes
  │   │     At t-end:  9 bytes (8.82%) in 1 blocks (6.25%), avg size 9 bytes
  │   │     Reads:     9 bytes (15.52%, 6.82/Minstr), 1/byte
  │   │     Writes:    13,491 bytes (15.52%, 10,229.89/Minstr), 1,499/byte
  │   │     Accesses: {
  │   │       [  0]  1500 〃 〃 〃 〃 〃 〃 〃 〃 
  │   │     }
  │   │     Allocated at {
  │   │       ^1: 0x1086CF: bm (sig.c:15)
  │   │       #2: 0x1086E9: b2 (sig.c:17)
  │   │       #3: 0x1088D7: main (sig.c:63)
  │   │     }
  │   │   }
  │   └── AP 1.2.3/3 {
  │         Writes:    0 bytes (0%, 0/Minstr), 0/byte
  │         Allocated at {
  │           [1 insignificant]
  │         }
  │       }
  ├─▼ AP 1.3/4 (2 children) {
  │     Writes:    13,491 bytes (15.52%, 10,229.89/Minstr), 562.13/byte
  │     Allocated at {
  │       #1: 0x10871D: cm (sig.c:21)
  │     }
  │   }
  │   ├── AP 1.3.1/2 {
  │   │     Total:     9 bytes (8.82%, 6.82/Minstr) in 1 blocks (6.25%, 0.76/Minstr), avg size 9 bytes, avg lifetime 169,315 instrs (12.84% of program duration)
  │   │     Max:       9 bytes in 1 blocks, avg size 9 bytes
  │   │     At t-gmax: 9 bytes (8.82%) in 1 blocks (6.25%), avg size 9 bytes
  │   │     At t-end:  9 bytes (8.82%) in 1 blocks (6.25%), avg size 9 bytes
  │   │     Reads:     9 bytes (15.52%, 6.82/Minstr), 1/byte
  │   │     Writes:    13,491 bytes (15.52%, 10,229.89/Minstr), 1,499/byte
  │   │     Accesses: {
  │   │       [  0]  1500 〃 〃 〃 〃 〃 〃 〃 〃 
  │   │     }
  │   │     Allocated at {
  │   │       ^1: 0x10871D: cm (sig.c:21)
  │   │       #2: 0x108912: main (sig.c:67)
  │   │     }
  │   │   }
  │   └── AP 1.3.2/2 {
  │         Writes:    0 bytes (0%, 0/Minstr), 0/byte
  │         Allocated at {
  │           [2 insignificant]
  │         }
  │       }
  └── AP 1.4/4 {
        Writes:    0 bytes (0%, 0/Minstr), 0/byte
        Allocated at {
          [1 insignificant]
        }
      }

AP significance threshold: (writes >= 434.71 bytes (0.5%)) && ((reads >= 1,000/byte) || (writes >= 1,000/byte))
`
//---------------------------------------------------------------------------
    }
  ]
};
tests.push(sig);

//---------------------------------------------------------------------------
// sig2 (doesn't corresponds to a .c file)
//---------------------------------------------------------------------------

let sig2 = {
  name: "sig2",
  input:
//---------------------------------------------------------------------------
{"dhatFileVersion":1
,"cmd":"subseqs"
,"pid":0
,"mi":10000,"ei":20000
,"aps":
 [{"tb":100,"tbk":1,"tli":1000
  ,"mb":100,"mbk":1
  ,"gb":100,"gbk":1
  ,"fb":0,"fbk":0
  ,"rb":0,"wb":0
  ,"acc":[-10,0]
  ,"fs":[1]
  }
 ,{"tb":101,"tbk":1,"tli":1000
  ,"mb":101,"mbk":1
  ,"gb":101,"gbk":1
  ,"fb":0,"fbk":0
  ,"rb":0,"wb":0
  ,"acc":[-10,0]
  ,"fs":[2]
  }
 ,{"tb":102,"tbk":1,"tli":1000
  ,"mb":102,"mbk":1
  ,"gb":102,"gbk":1
  ,"fb":0,"fbk":0
  ,"rb":0,"wb":0
  ,"acc":[-10,0]
  ,"fs":[3,4]
  }
 ,{"tb":103,"tbk":1,"tli":1000
  ,"mb":103,"mbk":1
  ,"gb":103,"gbk":1
  ,"fb":0,"fbk":0
  ,"rb":0,"wb":0
  ,"acc":[-10,0]
  ,"fs":[3,5]
  }
 ,{"tb":104,"tbk":1,"tli":1000
  ,"mb":104,"mbk":1
  ,"gb":104,"gbk":1
  ,"fb":0,"fbk":0
  ,"rb":0,"wb":0
  ,"acc":[-10,0]
  ,"fs":[3,6,7]
  }
 ,{"tb":105,"tbk":1,"tli":1000
  ,"mb":105,"mbk":1
  ,"gb":105,"gbk":1
  ,"fb":0,"fbk":0
  ,"rb":0,"wb":0
  ,"acc":[-10,0]
  ,"fs":[3,6,8]
  }
 ,{"tb":10,"tbk":1,"tli":1000
  ,"mb":10,"mbk":1
  ,"gb":10,"gbk":1
  ,"fb":0,"fbk":0
  ,"rb":0,"wb":0
  ,"acc":[-10,0]
  ,"fs":[3,6,9,10]
  }
 ,{"tb":106,"tbk":1,"tli":1000
  ,"mb":106,"mbk":1
  ,"gb":106,"gbk":1
  ,"fb":0,"fbk":0
  ,"rb":0,"wb":0
  ,"acc":[-10,0]
  ,"fs":[3,6,9,11]
  }
 ,{"tb":107,"tbk":1,"tli":1000
  ,"mb":107,"mbk":1
  ,"gb":107,"gbk":1
  ,"fb":0,"fbk":0
  ,"rb":0,"wb":0
  ,"acc":[-10,0]
  ,"fs":[3,6,9,12]
  }
 ]
,"ftbl":
 ["[root]"
 ,"a1()"
 ,"a2()"
 ,"a3()"
 ,"b1()"
 ,"b2()"
 ,"b3()"
 ,"c1()"
 ,"c2()"
 ,"c3()"
 ,"d1()"
 ,"d2()"
 ,"d3()"
 ]
}
//---------------------------------------------------------------------------
  ,
  outputs: [
    {
      label: "Total (blocks), tiny",
      expected:
//---------------------------------------------------------------------------
`\
Invocation {
  Command: subseqs
  PID:     0
}

Times {
  t-gmax: 10,000 instrs (50% of program duration)
  t-end:  20,000 instrs
}

▼ AP 1/1 (2 children) {
    Total:     9 blocks (100%, 450/Minstr), avg size 93.11 bytes
    Allocated at {
      #0: [root]
    }
  }
  ├─▼ AP 1.1/2 (2 children) {
  │     Total:     7 blocks (77.78%, 350/Minstr), avg size 91 bytes
  │     Allocated at {
  │       #1: a3()
  │     }
  │   }
  │   ├─▼ AP 1.1.1/2 (2 children) {
  │   │     Total:     5 blocks (55.56%, 250/Minstr), avg size 86.4 bytes
  │   │     Allocated at {
  │   │       #2: b3()
  │   │     }
  │   │   }
  │   │   ├─▼ AP 1.1.1.1/2 (2 children) {
  │   │   │     Total:     3 blocks (33.33%, 150/Minstr), avg size 74.33 bytes
  │   │   │     Allocated at {
  │   │   │       #3: c3()
  │   │   │     }
  │   │   │   }
  │   │   │   ├── AP 1.1.1.1.1/2 {
  │   │   │   │     Total:     2 blocks (22.22%, 100/Minstr), avg size 106.5 bytes
  │   │   │   │     Allocated at {
  │   │   │   │       [2 insignificant]
  │   │   │   │     }
  │   │   │   │   }
  │   │   │   └── AP 1.1.1.1.2/2 {
  │   │   │         Total:     10 bytes (1.19%, 500/Minstr) in 1 blocks (11.11%, 50/Minstr), avg size 10 bytes, avg lifetime 1,000 instrs (5% of program duration)
  │   │   │         Max:       10 bytes in 1 blocks, avg size 10 bytes
  │   │   │         At t-gmax: 10 bytes (1.19%) in 1 blocks (11.11%), avg size 10 bytes
  │   │   │         At t-end:  0 bytes (0%) in 0 blocks (0%), avg size 0 bytes
  │   │   │         Reads:     0 bytes (0%, 0/Minstr), 0/byte
  │   │   │         Writes:    0 bytes (0%, 0/Minstr), 0/byte
  │   │   │         Accesses: {
  │   │   │           [  0]  - - - - - - - - - - 
  │   │   │         }
  │   │   │         Allocated at {
  │   │   │           ^1: a3()
  │   │   │           ^2: b3()
  │   │   │           ^3: c3()
  │   │   │           #4: d1()
  │   │   │         }
  │   │   │       }
  │   │   └── AP 1.1.1.2/2 {
  │   │         Total:     2 blocks (22.22%, 100/Minstr), avg size 104.5 bytes
  │   │         Allocated at {
  │   │           [2 insignificant]
  │   │         }
  │   │       }
  │   └── AP 1.1.2/2 {
  │         Total:     2 blocks (22.22%, 100/Minstr), avg size 102.5 bytes
  │         Allocated at {
  │           [2 insignificant]
  │         }
  │       }
  └── AP 1.2/2 {
        Total:     2 blocks (22.22%, 100/Minstr), avg size 100.5 bytes
        Allocated at {
          [2 insignificant]
        }
      }

AP significance threshold: (total >= 0.05 blocks (0.5%)) && (total avg size <= 16 bytes)
`
//---------------------------------------------------------------------------
    }
  ]
};
tests.push(sig2);

//---------------------------------------------------------------------------
// Code
//---------------------------------------------------------------------------

function runTests() {
  let pre = appendElement(gTestingDiv, "pre");

  for (let [i, test] of tests.entries()) {
    let name = test.name;
    gData = test.input;

    for (let output of test.outputs) {
      // Set the sort metric.
      let label = output.label;
      let j = 0;
      let labelFound = false;
      for (let opt of gSelect.options) {
        if (gSelectData[opt.value].label == label) {
          gSelect.selectedIndex = j;
          labelFound = true;
          break;
        }
        j++;
      }
      assert(labelFound, "test label not found in gSelectData");

      // Build and display the tree.
      tryFunc(() => {
        gFilename = "TEST MODE";
        buildTree();
        displayTree();
      });

      // Compare actual text output against expected.
      let expected = output.expected;
      let actual = gMainDiv.textContent;

      let id = `Test ${i} - ${test.name} - ${label}`;

      if (expected !== actual) {
        // Test failed. Do a crude diff: find the line and column of the first
        // char that differs.
        let j = 0, line = 1, col = 1;
        while (expected[j] === actual[j]) {
          if (expected[j] === "\n") {
            line++;
            col = 1;
          } else {
            col++;
          }
          j++;
        }

        let s = `\
FAIL - ${id}

Expected length: ${expected.length}, actual length: ${actual.length}
First differing char at ${line}:${col}

EXPECTED OUTPUT
<<<
`;

        // Print line numbers for the expected output, because it makes it much
        // easier to find the first differing char.
        for (let [n, line] of expected.split('\n').entries()) {
          s += `${(n + 1).toString().padStart(3)} ${line}\n`;
        }

        s += ">>>";

        appendElementWithText(pre, "div", s);
        return;  // stop on the first failure
      }

      // Test passed.
      appendElementWithText(pre, "div", `PASS - ${id}`);
    }
  }

  clearMainDivWithText("All tests passed");
}

runTests();

