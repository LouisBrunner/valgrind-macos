#include <cstdlib>
#include <new>
#include <iostream>

class alignas(64) MyClass {
public:
    int i;
};

class OrdinaryClass {
public:
    int i;
};

int main() {
    // unsized versions
    MyClass* myClass = new MyClass;
    operator delete(myClass, std::align_val_t(64U));

    MyClass* myClass5 = new MyClass[5];
    operator delete [](myClass5, std::align_val_t(64U));

    // sized versions
    myClass = new MyClass();
    operator delete(myClass, 64U, std::align_val_t(64U));

    myClass5 = new MyClass[5];
    operator delete [](myClass5, 320U, std::align_val_t(64U));

    MyClass* myClassNt = new (std::nothrow) MyClass;
    operator delete(myClassNt, std::align_val_t(64U),  std::nothrow);

    MyClass* myClass5Nt = new (std::nothrow) MyClass[5];
    operator delete [](myClass5Nt, std::align_val_t(64U), std::nothrow);

    OrdinaryClass* oClass = new OrdinaryClass;
    // this is a limitation, VG does not use enough bits
    // to tell apart aligned and unaligned allocations
    // so new/aligned delete is not a mismatch
    operator delete(oClass, std::align_val_t(64U));
    oClass = new (std::nothrow) OrdinaryClass;
    //delete oClass;
    // changed the above delete because GCC generates
    // a sized delete (???) whilst clang generates an ordinary delete
    operator delete(oClass);
    oClass = new OrdinaryClass[5];
    delete [] oClass;
    oClass = new (std::nothrow) OrdinaryClass[5];
    delete [] oClass;
}

