class MyClass {
    int i;

};

int main() {
     MyClass* myClass = new MyClass();
    delete myClass;
    MyClass* myClass5 = new MyClass[5];
    operator delete[](myClass5, 5U*sizeof(MyClass));
    //delete [] myClass5;
}
