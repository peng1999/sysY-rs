int main(){
	int a = 10;
	int b = 20 + a;
	{
		int a = 1;
		b = a + b;
	}
	a = b * 3 - a;
	return a / 2;
}
