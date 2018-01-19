int b(){
    return 0;
}

int a(){
    if(0){
        return b() + 5;
    }
    else if(1){
        return b() + 3;
    } else return 1;
}

int main(){
    if(1 - 1)
        return b();
    else
        return a();
}
