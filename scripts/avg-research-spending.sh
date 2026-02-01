# Get R&D spending over revenue, sorted by market cap, for all companies in 2013.

# head -n 1000 resources/xrd.csv |
awk 'NF==5' resources/xrd.csv |
    awk '$3 != 0' |
    awk '$1 == 2013' |  # just one year so the list isn't super long
    awk '$5/$3 < 1' |  # remove weird outliers
    awk '
{
    # $4 = mkvalt, $5 = xrd
    printf "%s\t%s\t%s\n", $4, $5, $5/$3
}
' | sort -t, -nk1  # sort numerically by first column
