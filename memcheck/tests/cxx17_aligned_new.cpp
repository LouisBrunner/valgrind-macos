// See
// https://en.cppreference.com/w/cpp/memory/new/operator_new
// and
// https://en.cppreference.com/w/cpp/memory/new/operator_delete

// Some of these are only used in very special circumstances
// so rather then using new and delete expressions most calls
// here directly call the operators

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
    // THROWING
    // plain versions
    // cppreference new number 1
    OrdinaryClass* oClass = new OrdinaryClass();
    // cppreference delete number 1
    operator delete(oClass);

    // cppreference new number 2
    OrdinaryClass* oClass5 = new OrdinaryClass[5];
    // cppreference delete number 2
    operator delete[](oClass5);

    // aligned versions
    // cppreference new number 3
    MyClass* myClass = new MyClass();
    // cppreference delete number 3
    operator delete(myClass, std::align_val_t(64U));

    // cppreference new number 4
    MyClass* myClass5 = new MyClass[5];
    // cppreference delete number 4
    operator delete[](myClass5, std::align_val_t(64U));

    // sized versions
    oClass = new OrdinaryClass();
    // cppreference delete number 5
    operator delete(oClass, sizeof(OrdinaryClass));

    oClass5 = new OrdinaryClass[5];
    // cppreference delete number 6
    operator delete[](oClass5, sizeof(OrdinaryClass)*5);
    
    // sized aligned versions
    myClass = new MyClass();
    // cppreference delete number 7
    operator delete(myClass, sizeof(*myClass), std::align_val_t(64U));

    myClass5 = new MyClass[5];
    // cppreference delete number 8
    operator delete [](myClass5, sizeof(*myClass)*5, std::align_val_t(64U));

    // NOTHROW

    // cppreference new number 5
    oClass = new (std::nothrow) OrdinaryClass;
    delete oClass;

    // cppreference new number 6
    oClass5 = new (std::nothrow) OrdinaryClass[5];
    delete [] oClass5;

    // cppreference new number 7
    myClass = new (std::nothrow) MyClass;
    delete myClass;

    // cppreference new number 8
    myClass5 = new (std::nothrow) MyClass[5];
    delete [] myClass5;
}

