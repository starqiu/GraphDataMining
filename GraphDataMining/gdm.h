#include  <stdio.h>
#include <string>
#include <fstream>
#include <iostream>
#include "fileUtils.h"

using namespace std;

#define BASE_PATH   "D:/data/"
#define FILE_NAME  "liver_labeled_data_txt"
#define PERIOD_SAMPLE_COUNT  10 //each period has 10 samples
#define PERIOD_COUNT  5 //we have 5 periods:4wk,8wk,12wk,16wk,20wk
#define FEATURES_FILTERED_BY_SD 1000
#define CLUSTER_AMOUNT  3 

void divide_files_by_periods(string file_name);
void calc_and_filter_sd (char* file_name);
void sd_test();
void calc_pcc (char* file_name);
double calc_ci (double pccin,double pccout,double sd);
void pcc_test (char* file_name);
void dnb_test ();
void generate_dnb ();
void compare_to_example();
void array_add(int arr[],int arr_len,int num);
void array_init(int arr[],int arr_len);
vector<string> vector_cut(vector<string> vec,int cut_array[],int arr_len);
void printarray (int arg[], int length);

void divide_files_by_periods(string file_name){
	DataFrame* df = new DataFrame();
	read_table(file_name,df,' ',true);
	vector<string> geneIds;
	vector<vector<string>> rows = df->getRows();
	int arr_len = PERIOD_SAMPLE_COUNT+1;
	int cut_array[PERIOD_SAMPLE_COUNT+1];
	array_init(cut_array,arr_len);
	array_add(cut_array,arr_len,-PERIOD_SAMPLE_COUNT);
	string file_path("");
	for(int i = 0; i< PERIOD_COUNT;i++){
		array_add(cut_array,arr_len,PERIOD_SAMPLE_COUNT);
		cut_array[0] = 0;
		DataFrame* df_period = new DataFrame();
		for (vector<vector<string>>::iterator it = rows.begin(); it != rows.end(); ++it){		
			vector<string> row = vector_cut(*it,cut_array,arr_len);
			df_period->pushRow(row);
		}

		//construct file path string which contains number
		file_path = file_path + BASE_PATH + "matrix_table_";  
		char period[3]; // string which will contain the number
		sprintf ( period, "%d", (i+1)*4 ); // %d makes the result be a decimal integer
		file_path += period ;
		file_path += "wk.txt";

		write_table(file_path,df_period,'\t');
		file_path.clear();

		delete df_period;
	}
	delete df;
	
}

void calc_and_filter_sd (char*  file_name){

}

void sd_test(){

}

void calc_pcc (char* file_name){

}

double calc_ci (double pccin,double pccout,double sd){
	return sd * pccin /pccout;
}

void pcc_test (char* file_name){

}

void dnb_test (){

}

void generate_dnb (){

}

void compare_to_example(){

}

/**add num for each element*/
void array_add(int arr[],int arr_len,int num){
	for(int i=0 ; i<arr_len ; i ++ ){
		arr[i] += num;
	}
}

/**init the value of elem in array to equal with its index*/
void array_init(int arr[],int arr_len){
	for(int i=0 ; i<arr_len;i ++ ){
		arr[i] = i;
	}
}

/**return a vector which value in vec and its index in cut_array*/
vector<string> vector_cut(vector<string> vec,int cut_array[],int arr_len){
	vector<string> return_vector;
	for(int i=0;i<arr_len;i++){
		return_vector.push_back(vec[cut_array[i]]);
	}
	return return_vector;
}

void printarray (int arg[], int length) {
  for (int n=0; n<length; ++n)
    cout << arg[n] << ' ';
  cout << '\n';
}

