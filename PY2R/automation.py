##################################################################
# Steps for updating whiteboxR
# Step 1 - Delete the existing develop branch: git branch -D develop
# Step 2 - Create a new develop branch: git checkout -b develop
# Step 3 - Delete the old WhiteboxTools_linux_amd64.zip in the root folder if needed
# Step 4 - Run automation.py
# Step 5 - Run scripts to update internal datasets (data-raw/wbttools2.R; if needed)
# Step 6 - Update version number and RoxygenNote in DESCRIPTION
# Step 7 - Open whiteboxR.Rproj in RStudio and run build_check.R
# Step 8 - Commit and push changes
# Step 9 - Test installation from GitHub: devtools::install_github("opengeos/whiteboxR@develop")
# Step 10 - Merge pull request on GitHub
# Step 11 - Switch to master branch and pull updates: git checkout master | git pull
# Step 12 - Create R package: RStudio - Build - Build Source Package
# Step 13 - Upload to CRAN
##################################################################

import os
import re
import shutil
import zipfile
import urllib.request

# Extract function header


def function_header(line):
    if line.startswith("def"):
        line = line.replace("self, i,", "input,")
        line = line.replace("self, i=None,", "input,")
        line = line.replace("self, output, i=None,", "input, output,")
        line = line.replace(", i=None,", ", input=None,")
        line = line.replace("self, ", "")
        line = line.replace(
            "callback=None", "wd=NULL, verbose_mode=NULL, compress_rasters=NULL"
        )
        line = line.replace('function=""', 'FUN=""')
        line = line.replace("False", "FALSE")
        line = line.replace("True", "TRUE")
        line = line.replace("None", "NULL")
        line = line.replace("def ", "")
        # line = line.replace(":", ":")
        line = line.replace("And", "and")
        line = line.replace("Not", "not")
        line = line.replace("Or", "or")

    return line


# Extract function name
def function_name(line):
    line = line.strip()
    name = line[0: line.find("(")]
    return name


# Check if a string can be converted to float
def is_float(string):
    try:
        float(string)
        return True
    except ValueError:
        return False


# Generate R function block
def function_block(line, ff):
    line = line.strip()
    function_name = line[0: line.find("(")]
    start = line.find("(") + 1
    end = len(line) - 2
    argument = line[start:end]
    function_head = "wbt_" + function_name + " <- function(" + argument + ", command_only=FALSE) {"
    ff.write(function_head + "\n")
    ff.write("  wbt_init()" + "\n")
    ff.write('  args <- ""' + "\n")
    arguments = argument.split(",")
    for item in arguments:
        item = item.strip()
        if "=" not in item and '"' not in item:
            ff.write(
                '  args <- paste(args, paste0("--' + item +
                '=", wbt_file_path(' + item + ")))" + "\n"
            )
        elif "verbose" in item:
            continue
        elif not "compress_rasters" in item and "=FALSE" in item or "=TRUE" in item:
            para = item.split("=")[0]
            ff.write("  if (" + para + ") {" + "\n")
            ff.write('    args <- paste(args, "--' + para + '")' + "\n")
            ff.write("  }" + "\n")
        elif "verbose" not in item:
            para = item.split("=")[0]
            if '"' not in para and "compress_rasters" in para:
              ff.write("  if (!missing(" + para + ")) {" + "\n")
              ff.write(
                  '    args <- paste(args, paste0("--'
                  + para
                  + '=", '
                  + para
                  + "))"
                  + "\n"
              )
              ff.write("  }" + "\n")
            elif '"' not in para and "wd" in para or ("base" in para and "=NULL" in item):
              ff.write("  if (!missing(" + para + ")) {" + "\n")
              ff.write(
                  '    args <- paste(args, paste0("--'
                  + para
                  + '=", wbt_file_path('
                  + para
                  + ")))"
                  + "\n"
              )
              ff.write("  }" + "\n")
            elif "output" in para or "inputs" in para or "variant" in para or "streams" in para or "lakes" in para:
              ff.write("  if (!is.null(" + para + ")) {" + "\n")
              ff.write(
                  '    args <- paste(args, paste0("--'
                  + para
                  + '=", wbt_file_path('
                  + para
                  + ")))"
                  + "\n"
              )
              ff.write("  }" + "\n")
            elif '"' not in para:
              ff.write("  if (!is.null(" + para + ")) {" + "\n")
              ff.write(
                  '    args <- paste(args, paste0("--'
                  + para
                  + '=", '
                  + para
                  + "))"
                  + "\n"
              )
              ff.write("  }" + "\n")

    ff.write("  tool_name <- \""+function_name+"\"\n")
    # ff.write('  tool_name <- tool_name[!grepl("(whitebox|::)", tool_name)]' + "\n")
    ff.write("  wbt_run_tool(tool_name, args, verbose_mode, command_only)" + "\n")
    ff.write("}" + "\n")
    ff.write("\n\n")

# Extract function example usage
def function_example(fun_name):
    exe_path = os.path.join(os.path.dirname(dir_path), "WBT/whitebox_tools")
    cmd = exe_path + " --toolhelp=" + fun_name
    msg = os.popen(cmd).read().strip()
    ii = msg.find("usage:") + 7
    example = msg[ii:].split(">>")[1].strip()
    example = example.replace("-i=", "--input=")
    example = example.replace("-o=", "--output=")
    example = example.replace("--filter=25", "")
    example = example.replace(";", " ")  # semicolon in R is new line

    # function examples with problems
    problems = []
    problems.append("average_flowpath_slope")
    problems.append("average_upslope_flowpath_length")
    problems.append("directional_relief")
    problems.append("elevation_above_stream_euclidean")
    problems.append("fetch_analysis")
    problems.append("hillshade")
    problems.append("horizon_angle")
    problems.append("longest_flowpath")
    problems.append("max_upslope_flowpath_length")
    problems.append("num_inflowing_neighbours")
    problems.append("num_downslope_neighbours")
    problems.append("num_upslope_neighbours")
    problems.append("percent_elev_range")

    if fun_name in problems:
        example = example.replace("input", "dem")

    if fun_name == "write_function_memory_insertion":
        example = example.replace("-i", "--i")

    two_inputs_problems = []
    two_inputs_problems.append("cross_tabulation")
    two_inputs_problems.append("histogram_matching_two_images")
    two_inputs_problems.append("image_regression")
    two_inputs_problems.append("line_intersections")
    two_inputs_problems.append("write_function_memory_insertion")
    two_inputs_problems.append("lidar_kappa_index")
    two_inputs_problems.append("kappa_index")
    two_inputs_problems.append("join_tables")

    if fun_name in two_inputs_problems:
        example = example.replace("--i1", "--input1")
        example = example.replace("--i2", "--input2")

    if fun_name == "average_overlay":
        example = example.replace("input", "inputs")

    if fun_name == "corner_detection":
        example = example.replace("--sigma=2.0", "")
    if fun_name == "buffer_raster":
        example = example + "--size=5"
    if fun_name == "d_inf_pointer":
        example = example + "--output=output.tif"

    hyphen_problems = []
    hyphen_problems.append("difference")
    hyphen_problems.append("dissolve")
    hyphen_problems.append("intersect")
    if fun_name in hyphen_problems:
        example = example.replace("-input", "--input")

    if fun_name == "extract_raster_values_at_points":
        example = example.replace("-points", "--points")
        example = example.replace("input", "inputs")
    if fun_name == "find_parallel_flow":
        example = example + "--streams=streams.tif"

    duplicate_problems = []
    duplicate_problems.append("lidar_block_minimum")
    duplicate_problems.append("lidar_block_maximum")
    duplicate_problems.append("flightline_overlap")
    if fun_name in duplicate_problems:
        example = example.replace(
            "--input=file.las --output=outfile.tif", "", 1)
        example = example.replace("--palette=light_quant.plt", "")

    if fun_name == "k_nearest_mean_filter":
        example = example.replace("--filter=9 -k=5", "")
    if fun_name == "las_to_ascii":
        example = 'inputs="file1.las file2.las file3.las"'
    if fun_name == "lee_filter":
        example = '--input="image.tif" --output="output.tif"'

    params = example.split("--")
    ret = fun_name + "("
    for item in params:
        item = item.strip()
        if ("-r=" not in item) and ("wd=" not in item):
            values = item.split("=")
            if len(values) > 1:
                key = values[0]
                val = values[1]
                val = val.replace("'", '"')
                if is_float(val) == False:
                    val = val.replace('"', "")
                    val = '"' + val + '"'

                ret = ret + key + "=" + val + ", "
    ret = ret + "verbose_mode=TRUE)"
    return ret


toolboxes = {
    "# Data Tools #": "data_tools.R",
    "# GIS Analysis #": "gis_analysis.R",
    "# GIS Analysis/Distance Tools #": "gis_analysis_distance.R",
    "# GIS Analysis/Overlay Tools #": "gis_analysis_overlay.R",
    "# GIS Analysis/Patch Shape Tools #": "gis_analysis_patch_shape.R",
    "# Geomorphometric Analysis #": "terrain_analysis.R",
    "# Hydrological Analysis #": "hydro_analysis.R",
    "# Image Processing Tools #": "image_analysis.R",
    "# Image Processing Tools/Classification #": "image_analysis_classification.R",
    "# Image Processing Tools/Filters #": "image_analysis_filters.R",
    "# Image Processing Tools/Image Enhancement #": "image_analysis_enhancement.R",
    "# LiDAR Tools #": "lidar_analysis.R",
    "# Machine Learning #": "machine_learning.R",
    "# Math and Stats Tools #": "math_stat_analysis.R",
    "# Precision Agriculture #": "precision_agriculture.R",
    "# Stream Network Analysis #": "stream_network_analysis.R",
    "# Whitebox Utilities #": "whitebox_utilities.R"
}

dir_path = os.path.dirname(os.path.realpath(__file__))
wbt_py = os.path.join(dir_path, "whitebox_tools.py")
root_dir = os.path.dirname(dir_path)
WBT_dir = os.path.join(root_dir, "WhiteboxTools_linux_amd64", "WBT")

linux_tar = "WhiteboxTools_linux_amd64.zip"
tar_path = os.path.join(root_dir, linux_tar)
if not os.path.exists(tar_path):
    print("Downloading WhiteboxTools binary ...")
    url = "https://www.whiteboxgeo.com/WBT_Linux/WhiteboxTools_linux_amd64.zip"
    urllib.request.urlretrieve(url, tar_path)  # Download WhiteboxTools
else:
    print("WhiteboxTools binary already exists.")

if os.path.exists(WBT_dir):
    shutil.rmtree(WBT_dir)

print("Decompressing {} ...".format(linux_tar))
with zipfile.ZipFile(tar_path, "r") as tar_ref:
    tar_ref.extractall(root_dir)

shutil.copyfile(os.path.join( WBT_dir, "whitebox_tools.py"), wbt_py)

# Generate R functions with documentation
ff = None
add_example = False
desc = ""

with open(wbt_py) as f:
    lines = f.readlines()
    tbx = ""
    for index, line in enumerate(lines):
        if index > 700:
            line = line.strip()
            
            # Create an R script for each toolbox
            if line in toolboxes:
                tbx = line.replace("#", "").strip().replace(" ", "").replace("/", "")
                script_path = os.path.join(
                    dir_path, "scripts", toolboxes[line])
                ff = open(script_path, "w")
                print(script_path)
                
                # add code example to documentation
                if toolboxes[line] == "math_stat_analysis.R":
                    add_example = True
                else:
                    add_example = False

            if line.startswith("def"):
                title = line.replace("def", "").strip().split("(")[0]
                title = title.replace("_", " ")
                title = title[0].upper() + title[1:]
                ff.write("#' @title {}\n".format(title))
                ff.write("#'\n")
                i = 1
                while True:
                    doc_line = lines[index + i].strip()
                    if doc_line == '"""':
                        break
                    elif doc_line.startswith('"""'):
                        description = doc_line[3:] + "\n"
                        desc = description.strip().replace(".", "")
                        ff.write("#' @description {}".format(description))
                        ff.write("#'\n")
                    elif ("--" in doc_line) and (
                        doc_line.startswith("callback") == False
                    ):
                        # fix expected cross-reference (use code block)
                        doc_line = doc_line.replace("[0,1]", "`[0,1]`")
                        doc_line = doc_line.replace("[255, 255, 245]", "`[255, 255, 245]`")
                        doc_line = doc_line.replace("[255, 255, 245, 200]", "`[255, 255, 245, 200]`")
                        # fix reserved keywords used as argument names
                        if doc_line.startswith("function"):
                            doc_line = doc_line.replace("function ", "")
                            doc_line = "FUN " + doc_line
                        if doc_line.startswith("i --"):
                            doc_line = doc_line.replace("i --", "input --")
                        doc_line = doc_line.replace("-- ", "")
                        param = doc_line.replace("%", " percent")
                        param = re.sub("Input (raster|vector) file\\.", "Input \\1 file path. See `wbt_file_path()` for details.", param)
                        param = re.sub("Input (raster|vector) files\\.", "Input \\1 file paths, concatenated with `\",\"` or `\";\"`. See `wbt_file_path()` for details.", param)
                        ff.write("#' @param {}\n".format(param))
                    i = i + 1
                ff.write("#' @param wd Changes the working directory. Default: `NULL` will use the value in WhiteboxTools settings, see `wbt_wd()` for details.\n")
                ff.write(
                    "#' @param verbose_mode Sets verbose mode. If verbose mode is `FALSE`, tools will not print output messages. Default: `NULL` will use the value in WhiteboxTools settings, see `wbt_verbose()` for details.\n"
                )
                ff.write(
                    "#' @param compress_rasters Sets the flag used by 'WhiteboxTools' to determine whether to use compression for output rasters. Default: `NULL` will use the value in WhiteboxTools settings, see `wbt_compress_rasters()` for details.\n"
                )
                ff.write(
                    "#' @param command_only Return command that would be executed by `system()` rather than running tool. Default: `FALSE`.\n"
                )
                ff.write("#'\n")
                ff.write("#' @keywords {}\n".format(tbx))
                ff.write("#'\n")
                ff.write("#' @return Returns the tool text outputs.\n")
                ff.write("#' @export\n")
                # ff.write("#'\n")
                # ff.write("#' @examples\n")

                fun_head = function_header(line)
                # print(fun_head)

                if add_example:
                    fun_name = function_name(fun_head)
                    wbt_fun_name = "wbt_" + fun_name
                    fun_params = fun_head[fun_head.index("("):]

                    if (fun_params == "(input, output, verbose_mode=FALSE)") and (
                        fun_name != "raster_histogram"
                    ):
                        # print(fun_name)
                        output_name = fun_name + ".tif"
                        # print(fun_params)
                        # line0 = "#' " + "wbt_init()\n"
                        line1 = (
                            "#' "
                            + 'dem <- system.file("extdata", "DEM.tif", package="whitebox")'
                            + "\n"
                        )
                        line2 = (
                            "#' "
                            + wbt_fun_name
                            + "(input = dem, output = "
                            + "'output.tif'"
                            + ")"
                            + "\n"
                        )
                        ff.write("#'\n")
                        ff.write("#' @examples\n")
                        ff.write("#' \dontrun{\n")
                        ff.write(line1)
                        ff.write(line2)
                        ff.write("#' }\n")
                        # print(line1)
                        # print(line2)

                    skip_fun_tests = ["raster_histogram", "attribute_correlation",
                                      "conditional_evaluation", "crispness_index",
                                      "ks_test_for_normality", "rescale_value_range", "trend_surface"]
                    if (fun_params.startswith("(input, output") and not any(fun_name in s for s in skip_fun_tests)):
                        # write test scripts
                        test_file_name = "test-" + wbt_fun_name + ".R"
                        test_file_path = os.path.join(
                            dir_path, "tests", test_file_name)
                        f = open(test_file_path, "w")
                        f.write('context("{}")\n\n'.format(wbt_fun_name))
                        f.write('test_that("' + desc + '", {\n\n')
                        f.write("  skip_on_cran()\n")
                        f.write("  skip_if_not(check_whitebox_binary())\n")
                        f.write('  dem <- sample_dem_data(); skip_if(dem == "")\n')
                        f.write('  ret <- {}(input = dem, output = "output.tif")\n'.format(wbt_fun_name))
                        f.write('  expect_match(ret, "Elapsed Time")\n\n')
                        f.write("})\n")
                        print(test_file_path)
                        f.close()

                # fun_name = function_name(fun_head)
                # example = function_example(fun_name)
                # ff.write("#' {}\n".format(example))

                function_block(fun_head, ff)

ff.close()

scripts_path = os.path.join(dir_path, "scripts", "*.R")
R_scripts = os.path.join(os.path.dirname(dir_path), "R/")
cmd = "\\cp " + scripts_path + " " + R_scripts
os.system(cmd)

tests_path = os.path.join(dir_path, "tests", "*.R")
test_scripts = os.path.join(os.path.dirname(dir_path), "tests/testthat/")
cmd = "\\cp " + tests_path + " " + test_scripts
os.system(cmd)
