int main(){
	int a = 10;
	int b = 11;
	int c = 14;
	{
		int a = 12;
		int b = a;
		int c = 9;
		c = b;
	}
	b = c;
	return b;
}
