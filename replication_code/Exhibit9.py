"""

Exhibit9.py
-----------

Author  : Michael Dickens
Created : 2026-02-17

Calculate r^2 and generate plots based on Exhibit 9 from Israel et al. (2021), "Is (Systematic) Value Investing Dead?"

Plotting code was originally written by Claude Opus 4.6.

"""

import matplotlib.pyplot as plt
import numpy as np
from scipy.stats import ttest_ind

exhibit7_str = """
1987 0.16 5.0 0.49 13.3 0.28
1988 0.23 8.2 0.47 14.0 0.30
1989 0.16 6.1 0.47 14.4 0.28
1990 0.12 4.0 0.63 17.2 0.34
1991 0.19 7.1 0.61 17.0 0.37
1992 0.29 11.9 0.64 18.6 0.40
1993 0.31 12.8 0.66 20.4 0.42
1994 0.18 8.7 0.52 18.1 0.30
1995 0.16 6.8 0.54 17.2 0.30
1996 0.20 8.4 0.58 18.5 0.34
1997 0.34 17.3 0.64 21.0 0.43
1998 0.19 7.3 0.64 18.5 0.35
1999 0.03 1.2 0.60 15.2 0.25
2000 -0.02 -0.3 0.65 15.0 0.28
2001 0.44 22.3 0.55 16.3 0.50
2002 0.31 16.6 0.47 16.7 0.37
2003 0.10 6.0 0.41 15.9 0.27
2004 0.21 14.3 0.44 17.5 0.33
2005 0.19 13.6 0.48 22.2 0.39
2006 0.10 6.5 0.43 19.2 0.30
2007 0.08 4.9 0.46 16.7 0.28
2008 0.12 4.6 0.60 17.8 0.34
2009 0.11 4.6 0.42 17.1 0.26
2010 0.15 6.9 0.33 13.0 0.20
2011 0.10 5.8 0.41 15.8 0.25
2012 0.11 5.9 0.46 15.0 0.27
2013 0.11 6.7 0.40 13.6 0.26
2014 0.10 7.2 0.38 14.7 0.25
2015 0.09 5.5 0.45 17.3 0.34
2016 0.06 4.3 0.36 12.8 0.22
2017 0.12 8.6 0.42 14.2 0.26
2018 0.03 1.9 0.37 12.9 0.21
2019 0.02 0.9 0.45 12.0 0.20
2020 -0.07 -4.2 0.47 15.1 0.28
"""

exhibit9_str = """
1987 -0.01 -0.2 0.02 12.0 0.32 8.8 -0.34 -8.9
1988 0.06 2.7 0.02 10.6 0.41 12.5 -0.36 -10.2
1989 0.00 0.4 0.02 9.2 0.32 10.5 -0.32 -9.8
1990 -0.12 -3.9 0.02 10.7 0.26 7.9 -0.37 -11.4
1991 -0.02 -0.6 0.02 11.0 0.32 11.4 -0.34 -10.3
1992 0.11 4.6 0.02 16.0 0.36 14.5 -0.27 -9.1
1993 0.11 4.4 0.01 14.6 0.40 16.3 -0.31 -10.9
1994 0.03 1.4 0.02 16.1 0.31 12.4 -0.30 -11.4
1995 -0.02 -0.7 0.02 17.1 0.29 11.4 -0.32 -12.0
1996 0.02 0.7 0.02 14.4 0.31 13.0 -0.31 -11.9
1997 0.21 9.5 0.01 14.7 0.40 19.7 -0.21 -9.0
1998 0.04 1.5 0.01 13.2 0.27 10.4 -0.24 -8.7
1999 -0.17 -5.6 0.01 13.9 0.17 6.1 -0.33 -13.8
2000 -0.21 -7.3 0.01 14.6 0.09 4.1 -0.30 -15.3
2001 0.39 16.7 0.01 15.9 0.46 23.5 -0.08 -3.9
2002 0.25 11.7 0.01 15.4 0.36 15.0 -0.12 -4.3
2003 0.02 0.6 0.01 14.0 0.24 11.0 -0.23 -9.9
2004 0.08 5.3 0.01 12.0 0.36 20.7 -0.30 -14.4
2005 0.10 6.5 0.01 9.4 0.26 14.4 -0.17 -7.6
2006 -0.01 -0.4 0.01 7.6 0.22 11.4 -0.24 -11.0
2007 -0.03 -1.1 0.01 7.1 0.21 9.6 -0.24 -9.7
2008 -0.05 -1.7 0.01 6.2 0.25 8.5 -0.28 -9.0
2009 -0.01 0.0 0.01 2.6 0.29 9.7 -0.29 -8.9
2010 0.01 -0.1 0.00 3.9 0.43 16.2 -0.42 -15.2
2011 -0.01 -0.7 0.00 3.5 0.28 10.7 -0.28 -10.3
2012 0.02 0.9 0.01 5.1 0.22 8.9 -0.20 -7.6
2013 0.03 1.8 0.01 4.8 0.23 9.7 -0.20 -7.6
2014 0.02 1.6 0.01 5.8 0.24 10.4 -0.22 -9.0
2015 0.02 0.9 0.01 4.3 0.19 8.1 -0.17 -6.7
2016 0.01 0.5 0.01 5.3 0.16 7.0 -0.15 -6.3
2017 0.03 2.2 0.01 5.2 0.25 11.6 -0.22 -9.3
2018 -0.05 -3.7 0.01 6.0 0.17 7.5 -0.23 -9.8
2019 -0.06 -3.4 0.01 7.8 0.11 5.3 -0.17 -8.8
2020 -0.12 -7.0 0.01 9.1 -0.01 -0.5 -0.11 -4.6
"""

exhibit7 = np.array(
    [
        [float(s) for s in row.split(" ")]
        for row in exhibit7_str.split("\n")
        if row.strip() != ""
    ]
)
exhibit9 = np.array(
    [
        [float(s) for s in row.split(" ")]
        for row in exhibit9_str.split("\n")
        if row.strip() != ""
    ]
)
years = exhibit9[:, 0]
delta_f = exhibit9[:, 7]
delta_f_t = exhibit9[:, 8]
delta_fp = -exhibit9[:, 5]
delta_fp_t = -exhibit9[:, 6]


# Period masks
mask_pre = (years >= 1987) & (years <= 2006)
mask_post = (years >= 2007) & (years <= 2020)


def derive_n(t, rsqr):
    return t**2 * (1 - rsqr) / rsqr + 2


def derive_rsqr(t, n):
    return t**2 / (t**2 + (n - 2))


def print_rsqr(label, values):
    pre = values[mask_pre]
    post = values[mask_post]
    summary = ttest_ind(pre, post)
    print(
        f"{label} predictability:\n\t{pre.mean():.3f} -> {post.mean():.3f} (t-stat {summary.statistic:.2f}, p = {summary.pvalue:.3f})"
    )


# reverse-engineer Ns from Exhibit 7.
num_samples = derive_n(exhibit7[:, 4], exhibit7[:, 5])

print("")
delta_f_rsqr = derive_rsqr(delta_f_t, num_samples)
delta_fp_rsqr = derive_rsqr(delta_fp_t, num_samples)
print_rsqr("ΔF", delta_f_rsqr)
print_rsqr("Δ(F/P)", delta_fp_rsqr)
print("")


def make_plot(ax, years, values, label, ylabel):
    """Plot values with pre/post mean lines."""
    mean_pre = values[mask_pre].mean()
    mean_post = values[mask_post].mean()

    print(f"{label} {ylabel}:")
    print(f"  Mean 1987–2006: {mean_pre:.4f}")
    print(f"  Mean 2007–2020: {mean_post:.4f}")
    print()

    ax.plot(
        years,
        values,
        marker="o",
        linewidth=1.5,
        markersize=4,
        color="steelblue",
        label=ylabel,
    )
    ax.hlines(
        mean_pre,
        1987,
        2006,
        colors="tomato",
        linewidths=2,
        linestyles="--",
        label=f"Average 1987–2006 ({mean_pre:.3f})",
    )
    ax.hlines(
        mean_post,
        2007,
        2020,
        colors="#ff00bd",
        linewidths=2,
        linestyles="--",
        label=f"Average 2007–2020 ({mean_post:.3f})",
    )

    ax.set_xlabel("Year")
    ax.set_ylabel(ylabel)
    ax.legend()
    ax.grid(True, alpha=0.3)


#
# Create and save plots.
#

fig1, ax1 = plt.subplots(figsize=(10, 5))
ax1.axhline(0, color="black")
make_plot(ax1, years, delta_f_rsqr, "ΔF", "R²")
# make_plot(ax1, years, delta_f, "ΔF", "Slope")
fig1.tight_layout()
fig1.savefig("images/delta_f_plot.png", dpi=150)

fig2, ax2 = plt.subplots(figsize=(10, 5))
ax2.axhline(0, color="black")
make_plot(ax2, years, delta_fp, "Δ(F/P)", "Slope")
fig2.tight_layout()
fig2.savefig("images/delta_fp_plot.png", dpi=150)
