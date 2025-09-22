int next_collatz(int n) {
	if (n % 2 == 0) {
		return n/2;
	}
	return 3*n+1;
}

void wypisz(int od_czego, int ile) {
	while (ile > 0) {
		printInt(od_czego);
		od_czego = next_collatz(od_czego);
		ile--;
	}
	return;
}

wypisz(100, 10);
