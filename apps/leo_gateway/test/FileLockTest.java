import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.channels.FileChannel;
import java.nio.file.StandardOpenOption;
import java.lang.ProcessBuilder;
import java.lang.Process;
import java.io.OutputStream;
import java.io.InputStream;
import java.io.PrintStream;
import java.util.Scanner;
import java.nio.channels.FileLock;
import java.nio.channels.OverlappingFileLockException;
import java.nio.ByteBuffer;


public class FileLockTest {
    private static String filePath = "/mnt/leofs/testlock";
    private static long size = 1048576;

    public static void main(String[] args) throws IOException{
        if (args.length >= 1) {
            if (args[0].equals("worker")) {
                if (args.length >= 2) {
                    filePath = args[1];
                }
                worker();
            } else {
                filePath = args[0];
                server();
            }
        } else {
            server();
        }
    }

    private static void inst_lock(PrintStream out, Scanner scan, long offset, long end, boolean expected) throws IOException {
        out.format("lock %d %d%n", offset, end);
        out.flush();
        boolean ret = scan.nextBoolean();
        if (ret != expected) 
            throw new IOException("Not Matched, expected:" + expected + " ret:" + ret);
    }

    private static void server() throws IOException {
        Path path = Paths.get(filePath);
        FileChannel fc = FileChannel.open(path, StandardOpenOption.CREATE, StandardOpenOption.WRITE);
        fc.write(ByteBuffer.allocate(1), size - 1);
        fc.close();

        String [] cmd = {"java","FileLockTest","worker", filePath};
        ProcessBuilder process = new ProcessBuilder(cmd);
        Process proc1 = process.start();
        Process proc2 = process.start();

        OutputStream out1 = proc1.getOutputStream();
        OutputStream out2 = proc2.getOutputStream();
        InputStream in1 = proc1.getInputStream();
        InputStream in2 = proc2.getInputStream();

        PrintStream print1 = new PrintStream(out1);
        PrintStream print2 = new PrintStream(out2);
        Scanner scan1 = new Scanner(in1);
        Scanner scan2 = new Scanner(in2);

        System.out.println("Lock 1 for  20~ 50");
        inst_lock(print1, scan1, 20, 50, true);
        System.out.println("Lock 2 for   0~100");
        inst_lock(print2, scan2, 0, 100, false);
        System.out.println("Lock 2 for 101~200");
        inst_lock(print2, scan2, 101, 200, true);
    }

    private static boolean doLock(FileChannel fc, long offset, long end) throws IOException{
        try {
            FileLock lock = fc.tryLock(offset, end, false);
            return lock != null;
        } catch (OverlappingFileLockException e) {
            return true;
        }
    }

    private static void worker() throws IOException {
        Path path = Paths.get(filePath);
        FileChannel fc = FileChannel.open(path, StandardOpenOption.CREATE, StandardOpenOption.WRITE);

        Scanner scan = new Scanner(System.in);

        long offset;
        long end;
        boolean ret;

        while(scan.hasNextLine()) {
            String command = scan.next();
            if (command.equals("lock")) {
                offset = scan.nextLong();
                end = scan.nextLong();
                ret = doLock(fc, offset, end);
                System.out.println(ret);
                System.out.flush();
            }
            scan.nextLine();
        }
    }
}
