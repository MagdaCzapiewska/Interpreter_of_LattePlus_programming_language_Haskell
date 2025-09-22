int a = 1;
int b = 2;

int dodaj_a_i_b() {
	return a + b;
}

void wywolanie_dodawania_a_i_b() {
	int a = 11;
	int b = 12;
	printInt(dodaj_a_i_b());
	
	int dodaj_a_i_b() {
		return a + b;
	}
	printInt(dodaj_a_i_b());
	return;
}

wywolanie_dodawania_a_i_b();
