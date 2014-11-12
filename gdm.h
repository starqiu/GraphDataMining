#include  <stdio.h>
#include <string.h>
#include <fstream>
#include <iostream>

using namespace std;

#define BASE_PATH   "D:/data/"
#define FILE_NAME  "liver_labeled_data_txt"
#define PERIOD_SAMPLE_COUNT  10 //each period has 10 samples
#define PERIOD_COUNT  5 //we have 5 periods:4wk,8wk,12wk,16wk,20wk
#define FEATURES_FILTERED_BY_SD 1000
#define CLUSTER_AMOUNT  3 

void divide_files_by_periods(){
	printf("divide_files_by_periods");
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