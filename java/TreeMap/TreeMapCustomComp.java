import java.util.Comparator;
import java.util.TreeMap;
import java.util.HashMap;
import java.util.Map;

public class TreeMapCustomComp {
    public static void main(String a[]) {
        final HashMap<String, String> apps_pkgs = new HashMap<>();
        Comparator<String> apps_name_comparator = new Comparator<String>() {
            public int compare(String a1, String a2) {
                try {
                    String s1 = apps_pkgs.get(a1);
                    String s2 = apps_pkgs.get(a2);

                    return s1.compareToIgnoreCase(s2);
                } catch (Exception e) {
                    return a1.compareToIgnoreCase(a2);
                }
            }
        };
        
        apps_pkgs.put("android.froyo", "froyo");             
        apps_pkgs.put("com.android.donut", "donut");                                                                            
        apps_pkgs.put("com.nodroid.eclair", "eclair");
        apps_pkgs.put("pkg.android.kitkat", "kitkat");
        apps_pkgs.put("code.droid.gingerbread", "gingerbread");
        
        TreeMap<String, String> apps_list = new TreeMap<>(apps_name_comparator);
        apps_list.put("android.froyo", "froyo");             
        apps_list.put("com.android.donut", "donut");                                                                            
        apps_list.put("com.nodroid.eclair", "eclair");
        apps_list.put("pkg.android.kitkat", "kitkat");
        apps_list.put("code.droid.gingerbread", "gingerbread");

        System.out.println("Treemap: " + apps_list);
        
        for (Map.Entry<String, String> entry : apps_list.entrySet())
            System.out.println( entry.getKey() + ": " + entry.getValue());
    }
}


/** Output 
Treemap: {com.android.donut=donut, com.nodroid.eclair=eclair, android.froyo=froyo, code.droid.gingerbread=gingerbread, pkg.android.kitkat=kitkat}
com.android.donut: donut
com.nodroid.eclair: eclair
android.froyo: froyo
code.droid.gingerbread: gingerbread
pkg.android.kitkat: kitkat
*/
