import java.io.*;

/*
 * toRadiansInput was generated with the following CL:
 *
 * (with-open-file (out "toRadiansInput" :direction :output :if-does-not-exist :create :if-exists :supersede)
 *  (loop :for n :from -360 :to 361 :do (format out "~A~%" n))
 *  (loop :repeat 100000
 *        :do (format out "~A~%" (- (random 720d0) 360d0))))
 */

public class GenerateToRadiansData {
  public static void main(String[] args) {
    try {
      BufferedReader in = new BufferedReader(new FileReader(new File("toRadiansInput")));
      String line = null;
      System.out.println("(");
      while((line = in.readLine()) != null) {
        double deg = Double.parseDouble(line.replace("d0", ""));
        String radString = new Double(StrictMath.toRadians(deg)).toString();
        System.out.println(" (" + deg + "d0 " + (radString.contains("E") ? radString.replace("E", "d") : (radString + "d0")) + ")");
      }
      System.out.println(")");
    } catch (Exception e) {
      System.err.println("Odd problem: " + e);
    }
  }
}
