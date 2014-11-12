#include  <stdio.h>
#include "gdm.h"
#include <fstream>
#include <iostream>

using namespace std;

void divide_files_by_periods();
void calc_and_filter_sd (char* file_name);
void sd_test();
void calc_pcc (char* file_name);
double calc_ci (double pccin,double pccout,double sd);
void pcc_test (char* file_name);
void dnb_test ();
void generate_dnb ();
void compare_to_example();

int main(){
	char str[1024];

	const char * file_name = "example.txt"; 
	char file_path[40];
	strcpy(file_path,BASE_PATH);
	strcat(file_path,file_name);
	cout<< file_path<<endl;
	//Creates an instance of ofstream, and opens example.txt
	ofstream a_file ( file_path );
	// Outputs to example.txt through a_file
	a_file<<"This text will now be inside of example.txt";
	// Close the file stream explicitly
	a_file.close();
	//Opens for reading the file
	ifstream b_file ( file_path );
	//Reads one string from the file
	while(b_file.eof()){
		b_file.getline(str,1024) ;
		//Should output 'this'
		cout<< str <<"\n";
	}
	cin.get();    // wait for a keypress
	//cout<<BASE_PATH<<endl;
	//divide_files_by_periods();
	/*sd_test();
	dnb_test();
	generate_dnb();
	compare_to_example();*/
	return 0;
}
