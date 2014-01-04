/**
 * This is an application for aggregating CSV files
 * author:      Amir Ghaffari
 * @RELEASE project (http://www.release-project.eu/)
 */
 
import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.BufferedWriter;
import java.util.List;
import java.util.ArrayList;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.Enumeration;
import java.util.Map;
import java.util.Scanner;
import java.util.TreeMap;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.zip.ZipEntry;
import java.util.zip.ZipException;
import java.util.zip.ZipFile;
import java.util.zip.ZipOutputStream;

public class csv_tool
{
    static String resultFolderPath="results";
    public static Map<String, int[]> initial_except_columns()
    {
        Map<String, int[]> exception_columns = new TreeMap<String, int[]>();
        exception_columns.put("error.csv", new int [] {1});
        exception_columns.put("summary.csv", new int [] {1,2});
        exception_columns.put("global-register_latencies.csv", new int [] {1,2});
        exception_columns.put("global-unregister_latencies.csv", new int [] {1,2});
        exception_columns.put("global-whereis_latencies.csv", new int [] {1,2});
        exception_columns.put("spawning_latencies.csv", new int [] {1,2});
        exception_columns.put("remote-call_latencies.csv", new int [] {1,2});
        exception_columns.put("local-register_latencies.csv", new int [] {1,2});
        exception_columns.put("local-unregister_latencies.csv", new int [] {1,2});
        exception_columns.put("local-whereis_latencies.csv", new int [] {1,2});
        exception_columns.put("gen-server-call_latencies.csv", new int [] {1,2});
        exception_columns.put("fsm-server-call_latencies.csv", new int [] {1,2});
        return exception_columns;
    }

    public static Map<String, int[]> average_columns()
    {
        Map<String, int[]> average_columns = new TreeMap<String, int[]>();
        average_columns.put("global-register_latencies.csv", new int [] {4,5,6,7,8,9,10});
        average_columns.put("global-unregister_latencies.csv", new int [] {4,5,6,7,8,9,10});
        average_columns.put("global-whereis_latencies.csv", new int [] {4,5,6,7,8,9,10});
        average_columns.put("spawning_latencies.csv", new int [] {4,5,6,7,8,9,10});
        average_columns.put("remote-call_latencies.csv", new int [] {4,5,6,7,8,9,10});
        average_columns.put("local-register_latencies.csv", new int [] {4,5,6,7,8,9,10});
        average_columns.put("local-unregister_latencies.csv", new int [] {4,5,6,7,8,9,10});
        average_columns.put("local-whereis_latencies.csv", new int [] {4,5,6,7,8,9,10});
        average_columns.put("gen-server-call_latencies.csv", new int [] {4,5,6,7,8,9,10});
        average_columns.put("fsm-server-call_latencies.csv", new int [] {4,5,6,7,8,9,10});
        return average_columns;
    }

    public static void main(String [] args) 
    {
        String unzip_directory_name="";
        String baseFolderPath="test";
        if (args.length > 0) resultFolderPath=args[0];
        if (args.length > 1) baseFolderPath=args[1];
        System.out.print( resultFolderPath+"\n");
        System.out.print( baseFolderPath+"\n");
        
        String[] filesType = {"errors.csv", "summary.csv", "spawning_latencies.csv", "remote-call_latencies.csv", "global-whereis_latencies.csv", "global-unregister_latencies.csv", "global-register_latencies.csv", "local-register_latencies.csv", "local-unregister_latencies.csv", "local-whereis_latencies.csv", "gen-server-call_latencies.csv", "fsm-server-call_latencies.csv"};

        List<String> listOfZipFiles = searchOneLevelFiles(baseFolderPath,".zip");
        delete_file("logfile.txt");
        try {
            delete_directory(resultFolderPath);
            boolean success = (new File(resultFolderPath)).mkdirs();
            if (!success) {
                writeLog("Error! can't create "+resultFolderPath+ "folder");
                return;
            }
        } catch (IOException ex) {
            Logger.getLogger(csv_tool.class.getName()).log(Level.SEVERE, null, ex);
            writeLog("Error! can't delete "+resultFolderPath+ "folder");
            return;
        }
        for (int counter=0;counter<listOfZipFiles.size();++counter )
        {
            String fileName=fileName(listOfZipFiles.get(counter));
            String zip_file_name=listOfZipFiles.get(counter);
            unzip_directory_name=get_directory(listOfZipFiles.get(counter));

            try {
                delete_directory(unzip_directory_name);
                deleteFiles(searchOneLevelFiles(".",".csv"));
                unzip_directory_name=extractFolder(zip_file_name);
            } catch (ZipException ex) {
                Logger.getLogger(csv_tool.class.getName()).log(Level.SEVERE, null, ex);
            } catch (IOException ex) {
                Logger.getLogger(csv_tool.class.getName()).log(Level.SEVERE, null, ex);
            }

            for(int i=0; i < filesType.length; i++){
                List<String> fileList;
                fileList = searchFiles(unzip_directory_name,filesType[i]);
                String [] input_files  =fileList.toArray(new String[fileList.size()]);
                start_mixing (input_files, filesType[i],filesType[i]);
            }
            String joinedPath = new File(resultFolderPath, fileName+".zip").toString();
            delete_file(joinedPath);
            createZip(joinedPath,searchOneLevelFiles(".",".csv"));
        }
        deleteFiles(searchOneLevelFiles(".",".csv"));
    }

    static void deleteFiles(List<String> nameOfFiles) {
        
        for(int i=0;i<nameOfFiles.size();++i)
        {
            delete_file(nameOfFiles.get(i));
        }
    }    
    static List<String>  searchOneLevelFiles(String directory,String fileName) {
        List<String> findFiles = new ArrayList<String>();
        File dir = new File(directory);
        for (File file : dir.listFiles()) {
            if (file.getName().endsWith((fileName))) {
              findFiles.add(file.getAbsolutePath());
            }
        }
        return findFiles;
    }
    
    static List<String>  searchFiles(String directory,String fileName) {
        List<String> findFiles = new ArrayList<String>();
        File dir = new File(directory);
        for (File file : dir.listFiles()) {
            if (file.getName().endsWith((fileName))) {
              findFiles.add(file.getAbsolutePath());
            }
            if(file.isDirectory()){
              List<String> findSubFiles = searchFiles(file.getAbsolutePath(),fileName);
              findFiles.addAll(findSubFiles);
            }
        }
        return findFiles;
    }

    public static void start_mixing (String [] input_files, String output_file,String fileType) 
    {
        List<String[]> cols1;
        List<String[]> cols2;
        List<String[]> result = new ArrayList<String[]>();
        Map<String, int[]> except_columns=initial_except_columns();
        int[] average_columns=average_columns().get(fileType);
        int Number_of_exception=0;
        int Number_of_individual_exception=0;
        String Last_Error_File="";

        if (input_files.length<2)
        {
            System.out.print( "Error! Number of input file should be more than one for file type: "+fileType+" is: "+input_files.length+"\n" );
            writeLog("Error! Number of input file should be more than one for file type: "+fileType+" is: "+input_files.length+"\n");
            ++Number_of_exception;
            return;
        }

        try {
            cols1 = readFile(input_files[0]);
            for (int i=0;i<cols1.size();++i)
            {
                String[] row = cols1.get(i);
                result.add(row);
            }
        } catch (Exception e) {
            System.out.println("error*** error1 in file "+input_files[0]);
            writeLog("error*** error1 in file "+input_files[0]);
            e.printStackTrace();
            ++Number_of_exception;
            return;
        }
        for (int file_conter=1;file_conter<input_files.length;++file_conter)
        {
            try{
                Number_of_individual_exception=0;
                try {
                    cols2 = readFile(input_files[file_conter]);
                } catch (Exception e) {
                    System.out.println("error*** error2 in file "+input_files[file_conter]);
                    writeLog("error*** error2 in file "+input_files[file_conter]);
                    ++Number_of_exception;
                    e.printStackTrace();
                    continue;
                }
                for (int i=0;i<result.size();++i)
                {
                    double num1=0,num2=0;
                    String item1,item2;

                    for(int j=0;j<result.get(i).length;++j)
                    {
                        try{

                            boolean except=false;
                            int[] excpt_cols = except_columns.get(fileType);
                            for(int k=0;excpt_cols!=null && k<excpt_cols.length;++k)
                                if(j+1==excpt_cols[k])
                                {
                                    except=true;
                                    break;
                                }
                            if(except) continue;
                            if(isNumeric(result.get(i)[j]))
                            {
                                item1=result.get(i)[j];
                                if ((i==result.size()-1)&&(cols2.size()==result.size()-1)) continue;
                                    
                                if(isNumeric(cols2.get(i)[j]))
                                {
                                    item2=cols2.get(i)[j];
                                    num1 = Double.parseDouble(item1);  
                                    num2 = Double.parseDouble(item2); 
                                    result.get(i)[j]=Double.toString(num1+num2);
                                }
                            }
                        }
                        catch (Exception e) {
                            if(!Last_Error_File.equals(input_files[file_conter]))
                            {
                                writeLog("error3 in file "+input_files[file_conter]+" for i="+i+ " and j="+j);
                                System.out.println("error3 in file "+input_files[file_conter]+" for i="+i+ " and j="+j);
                                ++Number_of_exception;
                                e.printStackTrace();
                                Last_Error_File=input_files[file_conter];
                                Number_of_individual_exception=1;
                            }
                            else
                                ++Number_of_individual_exception;
                            continue;
                        }
                    }
                }
            }
            catch (Exception e) {
                System.out.println("error*** error5 in file "+input_files[file_conter]);
                writeLog("error*** error5 in file "+input_files[file_conter]);
                ++Number_of_exception;
                e.printStackTrace();
                continue;
            }
            if(Number_of_individual_exception>0)
            {
                writeLog("Number_of_individual_exception  in file "+input_files[file_conter]+": "+Number_of_individual_exception);
                writeLog("=====");
            }
        }

        for (int k=0;k<result.size();++k)
        {
            String item;
            double num=0,average;
            for (int i=0;average_columns!=null&&i<average_columns.length;++i)
            {
                int column=average_columns[i]-1;
                if(isNumeric(result.get(k)[column]))
                {
                    item=result.get(k)[column];
                    num = Double.parseDouble(item);
                    average=num/input_files.length;
                    result.get(k)[column]=Double.toString(average);
                }
            }
        }

        try{
            writeFile(output_file,result);
        } catch (IOException e) {
            System.out.println("error*** error4 in writing the result in file "+output_file);
            writeLog("error*** error4 in writing the result in file "+output_file);
            e.printStackTrace();
            return;
        }
        System.out.println("Number of files which could not be read: "+Number_of_exception);
        writeLog("Number of files which could not be read: "+Number_of_exception);
        writeLog("============================================================");
    }

    public static boolean isNumeric(String str)  
    {  
      try  
      {  
        double d = Double.parseDouble(str);  
      }  
      catch(NumberFormatException nfe)  
      {  
        return false;  
      }  
      return true;  
    }
    
    private static List<String[]> readFile(String fileName) throws IOException
    {
        List<String[]> values = new ArrayList<String[]>();
        Scanner s = new Scanner(new File(fileName));
        while (s.hasNextLine()) {
            String line = s.nextLine();
            values.add(line.split(","));
        }
        return values;
    }

    private static void writeFile(String fileName, List<String[]> table) throws IOException
    {
        delete_file(fileName);
        FileWriter fw = new FileWriter(fileName);
        PrintWriter pw = new PrintWriter(fw);
        for (int i=0;i<table.size();++i)
        {
            int len=table.get(i).length;
            for(int j=0;j<len;++j)
            {
                if(j<len-1)
                {
                    pw.print(table.get(i)[j]);
                    pw.print(",");
                }
                else
                {
                    pw.println(table.get(i)[j]);
                }
            }
        }
        //Flush the output to the file
        pw.flush();
        //Close the Print Writer
        pw.close();
        //Close the File Writer
        fw.close();        
    }

    static String get_directory(String zipFile)
    {
        String newPath = zipFile.substring(0, zipFile.length() - 4);
        return newPath;
    }

    static public String extractFolder(String zipFile) throws ZipException, IOException 
    {
        System.out.println(zipFile);
        int BUFFER = 2048;
        File file = new File(zipFile);

        ZipFile zip = new ZipFile(file);
        //String newPath = zipFile.substring(0, zipFile.length() - 4);
        String newPath = get_directory(zipFile);
        new File(newPath).mkdir();
        Enumeration zipFileEntries = zip.entries();

        // Process each entry
        while (zipFileEntries.hasMoreElements())
        {
            // grab a zip file entry
            ZipEntry entry = (ZipEntry) zipFileEntries.nextElement();
            String currentEntry = entry.getName();
            File destFile = new File(newPath, currentEntry);
            //destFile = new File(newPath, destFile.getName());
            File destinationParent = destFile.getParentFile();

            // create the parent directory structure if needed
            destinationParent.mkdirs();

            if (!entry.isDirectory())
            {
                BufferedInputStream is = new BufferedInputStream(zip
                .getInputStream(entry));
                int currentByte;
                // establish buffer for writing file
                byte data[] = new byte[BUFFER];

                // write the current file to disk
                FileOutputStream fos = new FileOutputStream(destFile);
                BufferedOutputStream dest = new BufferedOutputStream(fos,
                BUFFER);

                // read and write until last byte is encountered
                while ((currentByte = is.read(data, 0, BUFFER)) != -1) {
                    dest.write(data, 0, currentByte);
                }
                dest.flush();
                dest.close();
                is.close();
            }

            if (currentEntry.endsWith(".zip"))
            {
                // found a zip file, try to open
                extractFolder(destFile.getAbsolutePath());
            }
        }
        return newPath;
    }


public static void delete_directory(String directory)
    	throws IOException{
        File file = new File(directory);
    	//make sure directory exists
    	if(!file.exists()){
            System.out.println("Directory ("+directory+") does not exist.");
           return;
         }
    	if(file.isDirectory()){
 
    		//directory is empty, then delete it
    		if(file.list().length==0){
 
    		   file.delete();
    		   System.out.println("Directory is deleted : " 
                                                 + file.getAbsolutePath());
 
    		}else{
 
    		   //list all the directory contents
        	   String files[] = file.list();
 
        	   for (String temp : files) {
        	      //construct the file structure
        	      File fileDelete = new File(file, temp);
 
        	      //recursive delete
        	     delete_directory(fileDelete.getAbsolutePath());
        	   }
 
        	   //check the directory again, if empty then delete it
        	   if(file.list().length==0){
           	     file.delete();
        	     System.out.println("Directory is deleted : " 
                                                  + file.getAbsolutePath());
        	   }
    		}
 
    	}else{
    		//if file, then delete it
    		file.delete();
    		System.out.println("File is deleted : " + file.getAbsolutePath());
    	}
}

public static void delete_file(String fileName)
{
    File f = new File(fileName);
    if (!f.exists()) return;
    boolean success = f.delete();
    if (!success){
          System.out.println("Deletion file ("+fileName+") failed.");
          System.exit(0);
      }
}

  public static String fileName(String fullPath) { 
    int dot = fullPath.lastIndexOf(".");
    int sep = fullPath.lastIndexOf(File.separator);
    String ss=fullPath.substring(sep + 1, dot);
    return fullPath.substring(sep + 1, dot);
  }

  
  
    public static void createZip(String zipFile,List<String> nameOfFiles)   
    {   
        File zipFileName=new File(zipFile);
        File[] selected = new File[nameOfFiles.size()];
        for (int counter=0;counter<nameOfFiles.size();++counter )
        {
            selected[counter]=new File(nameOfFiles.get(counter));
        }
        try   
        {   
            byte[] buffer = new byte[1024];   
            ZipOutputStream out = new ZipOutputStream(new   
            FileOutputStream(zipFileName));   
            for (int i = 0; i < selected.length; i++)   
            {   
            FileInputStream in = new FileInputStream(selected[i]);   
            //out.putNextEntry(new ZipEntry(selected[i].getPath()));   
            out.putNextEntry(new ZipEntry(selected[i].getName()));   
            int len;   
            while ((len = in.read(buffer)) > 0)   
            {   
            out.write(buffer, 0, len);   
            }   
            out.closeEntry();   
            in.close();   
            }  
            out.close();   
        }    
        catch (IllegalArgumentException iae)   
        {    
            iae.printStackTrace();   
        }   
        catch (FileNotFoundException fnfe)   
        {   
            fnfe.printStackTrace();   
        }   
        catch (IOException ioe)   
        {   
            ioe.printStackTrace();   
        }   
    }
    public static void writeLog(String text)   
    {
        try{
        // Create file 
        String joinedPath = new File(resultFolderPath, "logfile.txt").toString();
        FileWriter fstream = new FileWriter(joinedPath,true);
        BufferedWriter out = new BufferedWriter(fstream);
        out.write(text+"\r\n");
        //Close the output stream
        out.close();
        }catch (Exception e){//Catch exception if any
        System.err.println("Error: " + e.getMessage());
        }
    }
}

