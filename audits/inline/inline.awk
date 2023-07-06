#!/usr/bin/awk -f

# type,len,read,decode

BEGIN {
  FS=",";
  CONVFMT="%.10g";
  SIZES[0]=64;
  SIZES[1]=128;
  SIZES[2]=256;
  SIZES[3]=512;
  PERC_D[1]=0.0;
  PERC_D[2]=0.2;
  PERC_D[3]=0.5;
  PERC_D[4]=0.75;
  PERC_D[5]=1.0;
  RUNTIME=14*1000000; # 14s -> us
}

{
    ty = $1;
    len = $2;
    read = $3;
    decode = $4;
    for (i in SIZES) {
      size=SIZES[i];
      if (len < size) {
        read_inline[size,ty]+=read;
        decode_inline[size,ty]+=decode;
        count_inline[size,ty]+=1;
      } else {
        read_ext[size,ty]+=read;
        decode_ext[size,ty]+=decode;
        count_ext[size,ty]+=1;
      }
    }
}

END {
  types[0] = "inode";
  types[1] = "contents";

  for (i in SIZES) {
    size = SIZES[i];

    print sprintf("%-8s", "type") " <" size sprintf("%-8s", "B ") " % >=" size "B";
    for (j in types) {
      ty = types[j];
      total = count_inline[size,ty] + count_ext[size,ty];
      per_inline = sprintf("%.1f%%", count_inline[size,ty] / total * 100);
      print sprintf("%-8s", ty) " " count_inline[size,ty] " (" per_inline ")\t" count_ext[size,ty];
    }

    print "< " size "B";
    print "---------------";
    print "type read decode total";
    total_r=0;total_d=0;
    for (j in types) {
      ty = types[j];
      print sprintf("%-8s", ty) " " read_inline[size,ty] " " decode_inline[size,ty] " " read_inline[size,ty]+decode_inline[size,ty];
      total_r+=read_inline[size,ty];
      total_d+=decode_inline[size,ty];
    }
    print sprintf("%-9s", "-") total_r " " total_d " " total_r+total_d;

    total_read_inline = total_r;
    total_decode_inline = total_d;

    print ">= " size "B";
    print "---------------";
    print "type read decode total";
    total_r=0;total_d=0;
    for (j in types) {
      ty = types[j];
      print sprintf("%-8s", ty) " " read_ext[size,ty] " " decode_ext[size,ty] " " read_ext[size,ty]+decode_ext[size,ty];
      total_r+=read_ext[size,ty];
      total_d+=decode_ext[size,ty];
    }
    print sprintf("%-9s", "-") total_r " " total_d " " total_r+total_d;

    total = total_read_inline + total_decode_inline + total_r + total_d;

    print ""
    print "%\t%-save-find\t%-save-runtime";
    for (p=1; p <= 5;p++) {
      perc = PERC_D[p];
      perc_total_decode_inline=perc*total_decode_inline;
      total_save = total_read_inline + perc_total_decode_inline;
      print (perc*100) "\t" sprintf("%.1f%%", (total_save/total)*100) "\t" sprintf("%.1f%%", (total_save/RUNTIME)*100)
    }

    print "";
  }
}
