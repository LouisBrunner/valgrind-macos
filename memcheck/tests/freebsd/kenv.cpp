#include <iostream>
#include <sstream>
#include <string>
#include <exception>
#include <memory>
#include <cerrno>
#include <kenv.h>
#include <unistd.h>

static long x0;

int main(int argc, char** argv)
{
   long *px{static_cast<long*>(malloc(2*sizeof(long)))};
   x0 = px[0];
   try
   {
      const size_t bufSize{1024};
      auto buf{std::make_unique<char[]>(bufSize)};
      std::string name{"bootfile"};
      int res{kenv(KENV_GET, name.c_str(), buf.get(), bufSize)};

      if (res == -1)
      {
         throw std::runtime_error("kenv get non-root");
      }

      if (argc > 1)
      {
         std::cout << buf.get() << '\n';
      }

      res = kenv(42*42, name.c_str(), buf.get(), bufSize);
      if (res == 0)
      {
         throw std::runtime_error("kenv get bogus action succeeded");
      }
      if (errno != EINVAL)
      {
         std::stringstream ss;
         ss << "kenv get bogus action wrong errno, expected " << EINVAL << " got " << errno;
         throw std::runtime_error(ss.str());
      }

      res = kenv(KENV_GET, "zyxxy", buf.get(), bufSize);
      if (res == 0)
      {
         throw std::runtime_error("kenv get bogus name succeeded");
      }
      if (errno != ENOENT)
      {
         std::stringstream ss;
         ss << "kenv get bogus name wrong errno, expected " << ENOENT << " got " << errno;
         throw std::runtime_error(ss.str());
      }

      res = kenv(KENV_DUMP, "this does not matter", nullptr, -1);
      if (res == -1)
      {
         throw std::runtime_error("kenv dump to get size non-root");
      }
      if (argc > 1)
      {
         std::cout << "dump size " << res << '\n';
      }
      auto dump_buf{std::make_unique<char[]>(res)};
      char* uninitCharStar;
      res = kenv(KENV_DUMP, uninitCharStar, dump_buf.get(), res);

      if (argc > 1)
      {
         // the buffer contains nul separated eleements, this will just print the first
         std::cout << dump_buf.get() << '\n';
      }

      if (0 == geteuid())
      {
         res = kenv(KENV_SET, "this", const_cast<char*>("that"), 5);
         if (res == -1)
         {
             throw std::runtime_error("kenv set root");
         }
         res = kenv(KENV_SET, "this", const_cast<char*>("thing"), 6);
         if (res == -1)
         {
             throw std::runtime_error("kenv set root");
         }
         res = kenv(KENV_UNSET, "this", const_cast<char*>("yes we have no bananas"), 42);
         if (res == -1)
         {
             throw std::runtime_error("kenv set root");
         }
      }
      else
      {
         // now try some things that will fail
         int uninitInt;
         res = kenv(KENV_SET, "this", const_cast<char*>("that"), uninitInt);
         if (res != -1)
         {
             throw std::runtime_error("kenv set non-root succeeded");
         }
         if (errno != EPERM)
         {
            std::stringstream ss;
            ss << "kenv get bogus action wrong errno, expected " << EPERM << " got " << errno;
            throw std::runtime_error(ss.str());
         }

         // checks all the args
         kenv(KENV_GET+x0, name.c_str()+x0, buf.get()+x0, 1024+x0);

         // now some memory errors
         char* freeName{new char[32]};
         sprintf(freeName, "%s", "blah");
         delete [] freeName;
         kenv(KENV_GET, freeName, buf.get(), 32);
         char* freeBuf{new char[32]};
         delete [] freeBuf;
         kenv(KENV_GET, name.c_str(), freeBuf, 32);
         kenv(KENV_GET, name.c_str(), buf.get(), 2*bufSize);
      }

   }
   catch (std::exception& e)
   {
      std::cout << "FAILED: " << e.what() << '\n';
      exit(-1);
   }
   free(px);
}
