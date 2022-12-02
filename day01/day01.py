#!/usr/bin/python3

def main():
    with open("input") as file:
        content = file.readlines()
        current_elf = 1
        current_cal = 0
        cals = []
        for line in content:
            if line == "\n":
                cals.append(current_cal)
                current_cal = 0
                current_elf += 1
            else:
                current_cal += int(line)
        cals.sort(reverse=True)
        print(f"Star 1 : {cals[0]}")
        print(f"Star 2 : {cals[0] + cals[1] + cals[2]}")

if __name__ == "__main__":
    main()
