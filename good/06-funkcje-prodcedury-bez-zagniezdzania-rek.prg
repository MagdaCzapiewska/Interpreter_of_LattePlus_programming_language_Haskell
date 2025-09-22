int fib(int n) {
	if (n == 0) {
		return 0;
	}
	if (n == 1) {
		return 1;
	}
	return fib(n - 1) + fib(n - 2);
}

void wypisz_fib(int n) {
	int i = 0;
	while (i <= n) {
		printInt(fib(i));
		i++;
	}
	return;
}
wypisz_fib(10);
