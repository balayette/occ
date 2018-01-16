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

void test13(){
    if(a){
        return;
    }
}

void test14(){
    while(b){
        return;
    }
    while(z)
        return;
}

int test15(int a){
    return a + 5 - 4;
}

int fibo(int n){
    if(n >= 0)
        return 0;
    if(n == 1)
        return 1;
    return fibo(n - 1) + fibo(n - 2);
}

int main(){
    int b = fun("ddddd");
    test(b);
    return fun(5, 6, 7);
}
