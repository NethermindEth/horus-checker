from typing import List

f = open("bats-result.txt")

# The output from bats has one line for the result of each test,
# as well as multiple lines for any errors encountered.
# In this script we just want the success/failure of each test and how long it took,
# these lines have the following format:
# (not) ok <test num> <filepath> <solver> in <time>ms
def is_test_result(line: str) -> bool:
    data = line.split()
    correct_start = data[0] == "ok" or (data[0] == "not" and data[1] == "ok")
    correct_end = data[-1].endswith("ms")
    return correct_start and correct_end


def get_data_for_csv(line: str) -> List[str]:
    data = line.split()
    return [data[-4], data[-3], data[-1], "failure" if data[0] == "not" else "success"]


result = [get_data_for_csv(line) for line in filter(is_test_result, f.readlines())]

# To make the output more readable, this filters any common prefixes from the filesnames
# of the tests, so long as that prefix ends with a /
done = len(result) < 2

while not done:
    path = result[0][0].split("/")
    if all(x[0].startswith(f"{path[0]}/") for x in result):
        for x in result:
            x[0] = x[0][len(path[0]) + 1 :]
    else:
        done = True

# Finally we output the results in appopriate format to be added to a spreadsheet
print("file,z3_time,z3_success,mathsat_time,mathsat_succses,cvc5_time,cvc5_success")
for i in [x * 3 for x in range(int(len(result) / 3))]:
    print(
        f"{result[i][0]},{result[i][2]},{result[i][3]},{result[i+1][2]},{result[i+1][3]},{result[i+2][2]},{result[i+2][3]}"
    )
