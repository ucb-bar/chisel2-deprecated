package Chisel;
import java.util.*;
import java.io.*;
import static java.lang.String.format;

public class SCWrapper{
    /*
   public static void main(String[] args){
      //Read in template
      String file = read_file("src/main/sysc/template.txt");

      //Generate replacements
      ComponentDef def = example_component_def2();
      String[][] replacements = generate_replacements(def);

      //Fill template and write out
      String filled = fill_template(file, replacements);
      write_file("generated/generated.cpp", filled);

      //Debug
      System.out.println(filled); //DEBUG      
   }
    */

    /*
   public static ComponentDef example_component_def(){
      ComponentDef def = new ComponentDef();
      def.ctype = "GCD_t";
      def.name = "GCD";
      def.entries.add(new CEntry("a", true, "dat_t<1>", "GCD__io_a", "GCD__io_r1", "GCD__io_v1"));
      def.entries.add(new CEntry("z", false, "dat_t<1>", "GCD__io_z", "GCD__io_rz", "GCD__io_vz"));
      return def;
   }

   public static ComponentDef example_component_def2(){
      ComponentDef def = new ComponentDef();
      def.ctype = "AddFilter_t";
      def.name = "AddFilter";
      def.entries.add(new CEntry("a", true, "dat_t<16>", "AddFilter__io_a", "AddFilter__io_ar", "AddFilter__io_av"));
      def.entries.add(new CEntry("b", false, "dat_t<16>", "AddFilter__io_b", "AddFilter__io_br", "AddFilter__io_bv"));
      return def;
   }
    */

   public static void genwrapper(ComponentDef c, String filename){
      //Read in template      
      String template = read_resource("template.txt");

      //Generate replacements
      String[][] replacements = generate_replacements(c);

      //Fill template and write out
      String filled = fill_template(template, replacements);
      write_file(filename, filled);

      //Debug
      System.out.println(filled);
   }

   public static void genwrapper(ComponentDef c, java.io.FileWriter filewriter){
      //Read in template      
      String template = read_resource("template.txt");

      //Generate replacements
      String[][] replacements = generate_replacements(c);

      //Fill template and write out
      String filled = fill_template(template, replacements);
      write_file(filewriter, filled);

      //Debug
      // System.out.println(filled);
   }

   public static String[][] generate_replacements(ComponentDef c){
      ArrayList<String[]> replacements = new ArrayList<String[]>();

      //Header file
      replacements.add(new String[]{"header_file", c.name + ".h"});

      //Component name and type
      replacements.add(new String[]{"name", "SCWrapped" + c.name});
      replacements.add(new String[]{"component_type", c.ctype});

      //I/O Fifos
      /*begin*/{
         String input_fifos = "";
         String output_fifos = "";
         for(CEntry e : c.entries){
            String decl = format("sc_fifo<%s >* %s;\n  ", e.type, e.name);
            if(e.is_input)
               input_fifos += decl;
            else
               output_fifos += decl;
         }
         replacements.add(new String[]{"input_fifos", input_fifos});
         replacements.add(new String[]{"output_fifos", output_fifos});
      }

      /*Initialize output fifos*/{
         //Pull out output fifos
         ArrayList<CEntry> fifos = new ArrayList<CEntry>();
         for(CEntry e : c.entries)
            if(!e.is_input) fifos.add(e);
         //Initialize
         String init = "";
         for(int i=0; i<fifos.size(); i++){
            init += format("%s = new sc_fifo<%s >(1);\n  ", fifos.get(i).name, fifos.get(i).type);
         }
         replacements.add(new String[]{"init_output_fifos", init});
      }

      /*Check input queues*/{
         //Pull out input fifos
         ArrayList<String> dvar = new ArrayList<String>();
         ArrayList<String> fvar = new ArrayList<String>();
         ArrayList<CEntry> fifos = new ArrayList<CEntry>();
         for(CEntry e : c.entries)
            if(e.is_input){
               dvar.add(genvar("dat"));
               fvar.add(genvar("filled"));
               fifos.add(e);
            }
         //Initialize
         String init = "";
         String fill = "";
         String check = "";
         for(int i=0; i<fifos.size(); i++){
            String type = fifos.get(i).type;
            String data = dvar.get(i);
            String filled = fvar.get(i);
            String in = fifos.get(i).name;
            String in_data = fifos.get(i).data;
            String ready = fifos.get(i).ready;
            String valid = fifos.get(i).valid;
            init += format("%s %s;\n    ", type, data);
            init += format("int %s = 0;\n    ", filled);
            fill += format("if(!%s){%s = %s->nb_read(%s);}\n      ", filled, filled, in, data);
            fill += format("c->%s = %s;\n      ", in_data, data);
            fill += format("c->%s = LIT<1>(%s);\n      ", valid, filled);
            check += format("if(c->%s.values[0]) %s = 0;\n      ", ready, filled);
         }
         replacements.add(new String[]{"input_buffers", init});
         replacements.add(new String[]{"fill_input", fill});
         replacements.add(new String[]{"check_input", check});
      }

      /*Check Output Queues*/{
         //Pull out output fifos
         ArrayList<CEntry> fifos = new ArrayList<CEntry>();
         for(CEntry e : c.entries)
            if(!e.is_input) fifos.add(e);
         //Check
         String check = "";
         String valid_output = "";
         for(int i=0; i<fifos.size(); i++){
            String valid = fifos.get(i).valid;
            String data = fifos.get(i).data;
            String ready = fifos.get(i).ready;
            String out = fifos.get(i).name;
            check += format("c->%s = LIT<1>(%s->num_free() > 0);\n      ", ready, out);
            valid_output += format("if(c->%s.values[0]) %s->nb_write(c->%s);\n    ", valid, out, data);
         }
         replacements.add(new String[]{"check_output", check});         
         replacements.add(new String[]{"valid_output", valid_output});         
      }
      
      return replacements.toArray(new String[0][]);
   }

   private static int unique_counter = 0;
   private static String genvar(String prefix){
      int c = unique_counter;
      unique_counter++;
      return prefix + c;         
   }

   public static String read_file(String filename){
      StringBuilder buffer = new StringBuilder();
      try{
         BufferedReader reader = new BufferedReader(new FileReader(filename));
         while(true){
            String line = reader.readLine();
            if(line == null) break;
            buffer.append(line);
            buffer.append("\n");
         }
         reader.close();
      }catch(IOException e){
         System.err.println("Error reading file " + filename);
         System.exit(-1);
      }
      return buffer.toString();
   }

   public static String read_resource(String resourcename){
      InputStreamReader resourcestreamReader = new InputStreamReader(SCWrapper.class.getResourceAsStream("/" + resourcename));
      String line = null;
      StringBuffer buffer = new StringBuffer("");
      try{
         BufferedReader reader = new BufferedReader(resourcestreamReader);
         while((line = reader.readLine()) != null) {
           buffer.append(line + "\n");
         }
         reader.close();
      }catch(IOException e){
         System.err.println("Error reading resource " + resourcename);
         System.exit(-1);
      }
      return buffer.toString();
   }

   public static void write_file(String filename, String file){
      try{
         BufferedWriter writer = new BufferedWriter(new FileWriter(filename));
         writer.write(file);
         writer.close();
      }catch(IOException e){
         System.err.println("Error writing file " + filename);
         System.exit(-1);
      }      
   }

   public static void write_file(java.io.FileWriter filewriter, String file){
      try{
         BufferedWriter writer = new BufferedWriter(filewriter);
         writer.write(file);
         writer.close();
      }catch(IOException e){
         System.err.println("Error writing file " + filewriter);
         System.exit(-1);
      }      
   }

   public static String fill_template(String template, String[][] replacements){
      for(String[] entry : replacements){
         String key = entry[0];
         String value = entry[1];
         String regex = "\\{\\!" + key + "\\!\\}";
         template = template.replaceAll(regex, value);
      }
      return template;
   }
}

class CEntry{
   public String name;
   public boolean is_input;
   public String type;
   public String data;
   public String ready;
   public String valid;
   public CEntry(String a_name, boolean input, String a_type, String a_data, String a_ready, String a_valid){
      name = a_name;
      is_input = input;
      type = a_type;
      data = a_data;
      ready = a_ready;
      valid = a_valid;
   }

   public String toString(){
      return
         name + " " +
         is_input + " " +
         type + " " +
         data + " " +
         ready + " " +
         valid;
   }
}

class ComponentDef{
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
