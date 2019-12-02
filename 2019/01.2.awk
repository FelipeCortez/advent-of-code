function module_to_fuel(mass) {
    fuel = (int (mass / 3)) - 2;
    return (fuel <= 0 ? 0 : fuel + module_to_fuel(fuel))
}

{ sum += module_to_fuel($1) }

END { print sum }
