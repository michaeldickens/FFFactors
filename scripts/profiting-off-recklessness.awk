NR==1 {
    # assign names based on header row
    for (i=1; i<=NF; i++) {
        f[$i] = i
    }
}
{
    mkt_ret = f["Mkt-RF"] + f["RF"]
    ls_ret = mkt_ret - (mkt_ret / (1 + mkt_ret))
    ctr += 1
}
