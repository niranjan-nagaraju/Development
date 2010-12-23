
<?php
/** Adapted from SOURCE: http://www.zend.com//code/codex.php?ozid=1226&single=1 */

 function checkIPorRange ($ip_address, $ip_to_test) {
    if (ereg("-",$ip_address)) {  
        // Range
		$ar = explode("-",$ip_address);
		printf("%s %s %ld %ld\n", $ar[0], $ar[1], ip2long($ar[0]), ip2long($ar[1]));
		$your_long_ip = ip2long($ip_to_test);

		printf("IP: %s -> %d\n", $ip_to_test, $your_long_ip);
        if ( ($your_long_ip >= ip2long($ar[0])) && ($your_long_ip <= ip2long($ar[1])) ) {
            return TRUE;
        }
    } else {
        // Single IP
        if ($ip_to_test == $ip_address) {
            return TRUE;
        }
    }
    return FALSE;
}

$ip_range = "198.152.0.0-198.152.4.0";
$ip_to_test = "198.152.1.32";

if (checkIPorRange($ip_range, $ip_to_test)) {
    print "IP OK\n";
}

?>
