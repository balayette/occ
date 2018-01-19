int b(){
    return 3 - 2 / 2;
}

int a(){
    return 2 + 2 - b();
}

int main(){
    return 2 * (a() * 2 + (2 + (2 - 1)) / 2);
}
