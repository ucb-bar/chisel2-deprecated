package Chisel;

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
