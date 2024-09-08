use super::*;

pub fn num_struct() -> Struct {
    let env = vars![
        to_str(to_str, 1),
        max(max, 2),
        min(min, 2),
        sqrt(sqrt, 1),
        sin(sin, 1),
        cos(cos, 1),
        tan(tan, 1),
        pow(pow, 2),
        floor(floor, 1),
        round(round, 1)
    ];

    Struct {
        id: None,
        env: Scope::from_vars(env),
    }
}
