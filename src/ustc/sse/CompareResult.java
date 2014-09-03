/*
 * ============================================================
 * The SSE USTC Software License
 * 
 * CompareResult.java
 * 2014年9月3日
 * 
 * Copyright (c) 2006 China Payment and Remittance Service Co.,Ltd        
 * All rights reserved.
 * ============================================================
 */
package ustc.sse;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;

/**
 * 实现功能： 比较生成的连个DNB文件中基因是否有重叠
 * <p>
 * date	        author            email		           notes<br />
 * ----------------------------------------------------------------<br />
 *2014年9月3日        邱星         starqiu@mail.ustc.edu.cn	    新建类<br /></p>
 *
 */
public class CompareResult {
	
	public static void compare(String path1,String path2) throws IOException{
		File file1 = new File(path1);
		BufferedReader br1 = new  BufferedReader(new FileReader(file1));
		File file2 = new File(path2);
		BufferedReader br2 = new  BufferedReader(new FileReader(file2));
		ArrayList<String> arr1 = new ArrayList<>();
		ArrayList<String> arr2 = new ArrayList<>();
		while(br1.ready()){
			arr1.add(br1.readLine());
		}
		while(br2.ready()){
			arr2.add(br2.readLine());
		}
		br1.close();
		br2.close();
		
		System.out.println(Arrays.toString(arr1.toArray()));
		System.out.println(Arrays.toString(arr2.toArray()));
		
		for (String stri1 : arr1) {
			if (arr2.contains(stri1)) {
				System.out.println(stri1);
			}
		}
		
	}
	
	public static void main(String[] args) {
		try {
			compare("D:/kp/深圳先进院/数据/论文数据/liver_DNB_t1.txt", "D:/kp/深圳先进院/数据/论文数据/liver_DNB_t4.txt");
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
}

