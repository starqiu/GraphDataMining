/*
 * ============================================================
 * The SSE USTC Software License
 * 
 * ShellUtils.java
 * 2014��8��29��
 * 
 * Copyright (c) 2006 China Payment and Remittance Service Co.,Ltd        
 * All rights reserved.
 * ============================================================
 */
package ustc.sse;

import java.io.BufferedReader;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;

/**
 * ʵ�ֹ��ܣ�java ִ��shell�����¼��־
 * <p>
 * date author email notes<br />
 * ----------------------------------------------------------------<br />
 * 2014��8��29�� ���� starqiu@mail.ustc.edu.cn �½���<br />
 * </p>
 *
 */
public class ShellUtils {
	/** ����·�� */
	private static final String basePath = "/tmp/";
	/** ��¼Shellִ��״������־�ļ���λ��(����·��) */
	private static final String executeShellLogFile = basePath
			+ "executeShell.log";

	public static int executeShell(String shellCommand) throws IOException {
		int success = 0;
		StringBuffer stringBuffer = new StringBuffer();
		BufferedReader bufferedReader = null;
		// ��ʽ������ʱ�䣬��¼��־ʱʹ��
		DateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:SS ");

		try {
			stringBuffer.append(dateFormat.format(new Date()))
					.append("׼��ִ��Shell���� ").append(shellCommand)
					.append(" \r\n");

			Process pid = null;
			String[] cmd = { "/bin/sh", "-c", shellCommand };
			// ִ��Shell����
			pid = Runtime.getRuntime().exec(cmd);
			if (pid != null) {
				stringBuffer.append("���̺ţ�").append(pid.toString())
						.append("\r\n");
				// bufferedReader���ڶ�ȡShell��������� 
				bufferedReader = new BufferedReader(new InputStreamReader(pid.getInputStream()), 1024);
				pid.waitFor();
			} else {
				stringBuffer.append("û��pid\r\n");
			}
			stringBuffer.append(dateFormat.format(new Date())).append(
					"Shell����ִ�����\r\nִ�н��Ϊ��\r\n");
			String line = null;
			// ��ȡShell��������ݣ������ӵ�stringBuffer��
			while (bufferedReader != null && (line = bufferedReader.readLine()) != null) {
				stringBuffer.append(line).append("\r\n");
			}
		} catch (Exception ioe) {
			stringBuffer.append("ִ��Shell����ʱ�����쳣��\r\n").append(ioe.getMessage())
					.append("\r\n");
		} finally {
			if (bufferedReader != null) {
				OutputStreamWriter outputStreamWriter = null;
				try {
					bufferedReader.close();
					// ��Shell��ִ������������־�ļ���
					OutputStream outputStream = new FileOutputStream(
							executeShellLogFile);
					outputStreamWriter = new OutputStreamWriter(outputStream,
							"UTF-8");
					outputStreamWriter.write(stringBuffer.toString());
				} catch (Exception e) {
					e.printStackTrace();
				} finally {
					outputStreamWriter.close();
				}
			}
			success = 1;
		}
		return success;
	}

	public static void main(String[] args) {
		try {
			executeShell("cd ..");
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
}