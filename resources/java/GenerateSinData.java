import java.io.*;

/*
 * See GenerateToRadiansData for toRadians input generation
 */

public class GenerateSinData {
  public static void main(String[] args) {
    try {
      BufferedReader in = new BufferedReader(new FileReader(new File("toRadiansInput")));
      String line = null;
      System.out.println("(");
      while((line = in.readLine()) != null) {
        double deg = Double.parseDouble(line.replace("d0", ""));
        String degString = new Double(deg).toString();
        String radString = new Double(StrictMath.sin(StrictMath.toRadians(deg))).toString();
        System.out.println(" (" +
            (degString.contains("E") ? degString.replace("E", "d") : (degString + "d0")) + " " +
            (radString.contains("E") ? radString.replace("E", "d") : (radString + "d0")) +
            ")");
      }
      System.out.println(")");
    } catch (Exception e) {
      System.err.println("Odd problem: " + e);
    }
  }
}
