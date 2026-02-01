# command to run: (from StockStrategy root)
# awk -F "\"*,\"*" -f awk/pick-cols.awk bigdata/Compustat_annual.csv > resources/xrd.csv

NR==1 {
    # assign names based on header row
    for (i=1; i<=NF; i++) {
        f[$i] = i
    }
}
# print only desired columns
{ print $(f["fyear"]), $(f["gvkey"]), $(f["revt"]), $(f["mkvalt"]), $(f["xrd"]) }
