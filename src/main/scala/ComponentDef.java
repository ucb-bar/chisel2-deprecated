package Chisel;
import java.util.*;

public class ComponentDef{
   public String ctype;
   public String name;   
   public ArrayList<CEntry> entries;
   public ComponentDef(){
      entries = new ArrayList<CEntry>();
   }

   public String toString(){
      String accum = ":[";
      for(CEntry e:entries){
         accum += e + ", ";
      }
      accum += "]";

      return "Component " + ctype + " " + name + entries;
   }
}
