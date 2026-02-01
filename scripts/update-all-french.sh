#!/bin/bash

#
# Download all Ken French data files and convert them to a usable format.
#
# Warning: Developed_ex_US_Mom_Factor is a special case that this script doesn't
# work for.
#

declare -A name_to_url
name_to_url=(
    # not all files are included, only the ones I'm likely to use frequently
    ["6_Portfolios_Size_Momentum"]="6_Portfolios_ME_Prior_12_2"
    ["6_Portfolios_Size_Profitability"]="6_Portfolios_ME_OP_2x3"
    ["6_Portfolios_Size_B-M"]="6_Portfolios_2x3"
    ["25_Portfolios_B-M_Investment"]="25_Portfolios_BEME_INV_5x5"
    ["25_Portfolios_Size_B-M"]="25_Portfolios_5x5"
    ["25_Portfolios_Size_Investment"]="25_Portfolios_ME_INV_5x5"
    ["25_Portfolios_Size_Momentum"]="25_Portfolios_ME_Prior_12_2"
    ["25_Portfolios_Size_Variance"]="25_Portfolios_ME_VAR_5x5"
    ["100_Portfolios_Size_B-M"]="100_Portfolios_10x10"
    ["3_Factors_Asia_Pacific_ex_Japan"]="Asia_Pacific_ex_Japan_3_Factors"
    ["3_Factors_Developed_ex_US"]="Developed_ex_US_3_Factors"
    ["3_Factors_Europe"]="Europe_3_Factors"

    # these don't appear to exist anymore
    # ["3_Factors_Global"]="Global_3_Factors"
    # ["3_Factors_Global_ex_US"]="Global_ex_US_3_Factors"

    ["3_Factors_Japan"]="Japan_3_Factors"
    ["3_Factors"]="F-F_Research_Data_Factors"
    ["5_Factors"]="F-F_Research_Data_5_Factors_2x3"
    ["5_Factors_Developed_ex_US"]="Developed_ex_US_5_Factors"

    # TODO: these need a special case because the contents are formatted differently
    # ["Breakpoints_B-M"]="BE-ME_Breakpoints"
    # ["Breakpoints_CF-P"]="CF-P_Breakpoints"
    # ["Breakpoints_E-P"]="E-P_Breakpoints"
    # ["Breakpoints_Momentum"]="Prior_2-12_Breakpoints"

    ["Momentum_Factor"]="F-F_Momentum_Factor"

    # TODO: this is a weird special case, I have code to handle it but it's not working
    ["Momentum_Factor_Developed_ex_US"]="Developed_ex_US_Mom_Factor"

    ["Portfolios_B-M"]="Portfolios_Formed_on_BE-ME"
    ["Portfolios_CF-P"]="Portfolios_Formed_on_CF-P"
    ["Portfolios_D-P"]="Portfolios_Formed_on_D-P"
    ["Portfolios_E-P"]="Portfolios_Formed_on_E-P"
    ["Portfolios_Momentum"]="10_Portfolios_Prior_12_2"
    ["Portfolios_Profitability"]="Portfolios_Formed_on_OP"
    ["Portfolios_Size"]="Portfolios_Formed_on_ME"
    ["Portfolios_Accruals"]="Portfolios_Formed_on_AC"
    ["Portfolios_Beta"]="Portfolios_Formed_on_BETA"
    ["Portfolios_Variance"]="Portfolios_Formed_on_VAR"
    ["Developed_ex_US_25_Portfolios_Size_Momentum"]="Developed_ex_US_25_Portfolios_ME_Prior_12_2"
    ["Developed_ex_US_25_Portfolios_Size_B-M"]="Developed_ex_US_25_Portfolios_ME_BE-ME"
)

# iterate thru name_to_url and create unzipped_name_to_name
declare -A unzipped_name_to_name
for key in "${!name_to_url[@]}"
do
    # special case where unzipped name does not match URL
    if [ $key == "Momentum_Factor_Developed_ex_US" ]
    then
        unzipped_name_to_name["Developed_ex_US_MOM_Factor"]=$key
    else
        unzipped_name_to_name[${name_to_url[$key]}]=$key
    fi
done

url_prefix="https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/"

skip_download=false
verbose=false

for arg in "$@"
do
    if [ $arg == "--skip-download" ]; then
        skip_download=true
    elif [ $arg == "--verbose" ] || [ $arg == "-v" ]; then
        verbose=true
    fi
done

if [ $skip_download == true ]
then
    # Use this option if you've already downloaded the files and just want to
    # convert them to the usable format
    echo "Skipping download"
else
    # iterate thru all_files and download any missing files
    downloaded_any=false
    for file in resources/French/*.csv
    do
        # skip non-csv files
        if [[ $file != *.csv ]]; then
            continue
        fi
        # skip any file with "Equal_Wt" in the name to prevent redundant downloads
        if [[ $file == *Equal_Wt* ]]; then
            continue
        fi

        # Don't update the file if it's already up to date. Determine by checking
        # the date in the last row
        up_to_date_date=$(date -d "3 months ago" +%Y%m)
        last_line=$(tail -n 1 $file)
        line_date=$(echo $last_line | cut -d ',' -f 1)
        if [ $line_date -ge $up_to_date_date ]; then
            if [ $verbose == true ]; then
                echo "Skipping up-to-date file: $file"
            fi
            continue
        fi

        # replace .csv in file with _CSV.zip
        core_name=$(basename $file \
                        | sed 's/_Value_Wt//' \
                        | sed 's/\.csv//' \
                )
        if [ -n "${name_to_url[$core_name]}" ]; then
            echo "subscripting $core_name"
            zip_file="${name_to_url[$core_name]}_CSV.zip"
        else
            # zip_file="${core_name}_CSV.zip"
            if [ $verbose == true ]; then
                echo "Skipping file with no name translation: $file"
            fi
            continue
        fi
        wget -q --show-progress $url_prefix$zip_file -P resources/French/downloads
        if [ $? -ne 0 ]; then
            echo "Not found in Ken French Data Library: $zip_file"
            continue
        fi
        downloaded_any=true
    done

    if [ $downloaded_any == false ]
    then
        echo "No files downloaded."
    else
        unzip -o resources/French/downloads/\*.zip -d resources/French/downloads
        gio trash resources/French/downloads/*.zip
    fi
fi

echo ""

# exit on failure after this point
set -e

for file in resources/French/downloads/*
do
    # skip files containing Value_Wt or Equal_Wt in the name
    if [[ $file == *Value_Wt* ]] || [[ $file == *Equal_Wt* ]]
    then
        continue
    fi
    base_name=$(basename $file)
    core_name=$(echo $base_name | sed 's/.CSV//' | sed 's/.csv//')

    if [ -n "${unzipped_name_to_name[$core_name]}" ]
    then
        echo "Renaming $base_name to ${unzipped_name_to_name[$core_name]}.CSV"
        core_name=${unzipped_name_to_name[$core_name]}
        base_name=$core_name.CSV
        mv $file resources/French/downloads/$core_name.CSV
    fi

    runhaskell scripts/ManageFrench.hs resources/French/downloads/$base_name --quiet
    if [ $? -eq 0 ]
    then
        echo "Processed $base_name into a pure csv"
    fi
done

mv resources/French/downloads/*.csv resources/French/
gio trash resources/French/downloads/*.CSV
