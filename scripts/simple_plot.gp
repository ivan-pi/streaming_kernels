# Set terminal output (PDF is usually best for reports, 
# but you can change to 'pngcairo' if needed)
set terminal pngcairo font "Verdana,"
set output 'scaling_results.png'

# Formatting the plot
set title "Bandwidth vs Threads"
set xlabel "Number of Threads"
set ylabel "Rate (GB/s)"
set grid
set key bottom right

# Force Y-axis to start at 0 and auto-scale the maximum
set yrange [0:*]


# Plotting column 1 (Threads) vs column 3 (Rate)
# We use 'for' to iterate through files 1 to 5
plot for [i=1:5] sprintf("result_bs%d.txt", i) \
     using 1:3 skip 2 with linespoints ls i title sprintf("Test BS%d", i)