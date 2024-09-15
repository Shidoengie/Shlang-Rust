use super::*;

pub fn num_struct() -> Struct {
    let env = vars![
        to_str(1),
        max(2),
        min(2),
        sqrt(1),
        sin(1),
        cos(1),
        tan(1),
        pow(2),
        floor(1),
        round(1)
    ];

    Struct {
        id: None,
        env: Scope::from_vars(env),
    }
}
