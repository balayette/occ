int b(){
    return 3 - 2 / 2;
}

int a(){
    return 2 + 2 - b();
}

int main(){
    return a() * 2 + 4 / 2 + 1;
}
