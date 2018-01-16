void test1(){
    return;
}

int test2(){
    return 2;
}

int test3(int a){
    return 1;
}

int test5(int a, int b){
    return 1;
}

int test6(){
    int a = 5;
    string b = 6;
    return 1;
}

int test7(){
    int b = test6(5);
    return 2;
}

int test8(){
    return test7();
}

int test9(){
    int a = test7()[0];
    return 1;
}

int test10(){
    int a = test7(1, 2, 4)[0];
    return 1;
}

int test11(){
    int a = *b;
    return *a;
}

int test12(){
    int a[5] = funcall();
    string a[] = funcall();
    return 0;
}

int main(){
    int b = fun("ddddd");
    test(b);
    return fun(5, 6, 7);
}
