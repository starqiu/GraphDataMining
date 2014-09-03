/*
 * ============================================================
 * The SSE USTC Software License
 * 
 * FileReaddAndWrite.java
 * 2014��8��26��
 * 
 * Copyright (c) 2006 China Payment and Remittance Service Co.,Ltd        
 * All rights reserved.
 * ============================================================
 */
package ustc.sse;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;

/**
 * ʵ�ֹ��ܣ� �ļ���д,�Ӵ������г�ȡǰn�������Ա㿪��
 * <p>
 * date author email notes<br />
 * ----------------------------------------------------------------<br />
 * 2014��8��26�� ���� starqiu@mail.ustc.edu.cn �½���<br />
 * </p>
 *
 */
public class FileReaddAndWrite {

	public static void readPartFileAndWriteToNewFile(String path, int start, int end,String outputFileName)
			throws IOException {
		
		if (start<=0 || start >end) {
			return;
		}
		File bigFile = new File(path);
		BufferedReader bigFileReader = new BufferedReader(new FileReader(bigFile));
		String smallFileName = bigFile.getParent()+File.separator+(outputFileName!=null?outputFileName: (start+"_"+end+"rows.txt"));
		File smallFile = new File(smallFileName);
		if (!smallFile.exists()) {
			smallFile.createNewFile();
		}
		BufferedWriter smallFileWriter = new BufferedWriter(new FileWriter(smallFile));
		
		int skipIndex = start ;
		while(skipIndex >1 ){
			bigFileReader.readLine();
			skipIndex--;
		}
		int currentIndex = start;
		String line ="";
		while (currentIndex <= end) {
			if (currentIndex != start) {
				smallFileWriter.newLine();
			}
			line =bigFileReader.readLine();
			System.out.println(line);
			smallFileWriter.write(line);
			currentIndex++;
		}
		bigFileReader.close();
		smallFileWriter.close();
		
	}
	
	public static void  readPartFileAndWriteToNewFile(String path, int end,String outputFileName) throws IOException{
		readPartFileAndWriteToNewFile(path, 1, end,outputFileName);
	}
	
	public static void  readPartFileAndWriteToNewFile(String path,int start, int end) throws IOException{
		readPartFileAndWriteToNewFile(path, start, end,null);
	}
	
	public static void  readPartFileAndWriteToNewFile(String path, int end) throws IOException{
		readPartFileAndWriteToNewFile(path, end,null);
	}

	/**
	 * ��ȡGSExxxxx_series_matrix.txt�ļ��е� series_matrix_table д��Ŀ���ļ���,���ɵ�Ŀ���ļ�����Դ�ļ���ͬһĿ¼��
	 * @param path
	 * @param outputFileName ����ļ���
	 * @throws IOException 
	 */
	public static void readMatrixTableAndOutPut(String path,String outputFileName) throws IOException{
		
		File sourceFile = new File(path);
		BufferedReader br = new  BufferedReader(new FileReader(sourceFile));
		File outputFile = new File(sourceFile.getParent()+File.separator+outputFileName);
		if (!outputFile.exists()) {
			outputFile.createNewFile();
		}
		BufferedWriter wr = new BufferedWriter(new FileWriter(outputFile));
		
		String line = "";
		final String TABLE_START = "!series_matrix_table_begin";
		final String TABLE_END = "!series_matrix_table_end";
		line = br.readLine();
		int index = 0;
		while(!TABLE_START.equals(line)){
			line = br.readLine();
			System.out.println((index++)+" : "+line);
		}
		
		line = br.readLine();//title
		//System.out.println(line);
		boolean notStartFlag  = false;//to avoid outputing a br in the first line
		while(!TABLE_END.equals(line)){
			if(notStartFlag){
				wr.newLine();
			}else {
				notStartFlag = true;
			}
			wr.write(line);
			line = br.readLine();
		}
		br.close();
		wr.close();
	}
	
	/**
	 * @param args
	 */
	public static void main(String[] args) {
		//String bigFilepath = "D:/kp/�����Ƚ�Ժ/����/GPL1355_family.soft";
		String seriesPath = "D:/kp/�����Ƚ�Ժ/����/geo/series/GSE13nnn/GSE13270/matrix/GSE13270_series_matrix.txt";
		try {
			
			//FileReaddAndWrite.readPartFileAndWriteToNewFile(bigFilepath,58, 16206,"Platform_sample_id.csv");
			//FileReaddAndWrite.readPartFileAndWriteToNewFile(bigFilepath,16207, 16707,"Platform_series_id.csv");
			//FileReaddAndWrite.readPartFileAndWriteToNewFile(bigFilepath,16726, 16736);
			readMatrixTableAndOutPut(seriesPath,"series_matrix_table.txt");
			/*FileReaddAndWrite.readPartFileAndWriteToNewFile(bigFilepath, 20000);
			FileReaddAndWrite.readPartFileAndWriteToNewFile(bigFilepath, 30000);
			FileReaddAndWrite.readPartFileAndWriteToNewFile(bigFilepath, 40000);
			FileReaddAndWrite.readPartFileAndWriteToNewFile(bigFilepath, 50000);
			FileReaddAndWrite.readPartFileAndWriteToNewFile(bigFilepath, 100000);*/
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

}
