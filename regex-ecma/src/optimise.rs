

use crate::ast::*;

pub fn optimise(pattern: &mut Pattern){
    optimise_disjunction(&mut pattern.disjunction);
}

pub fn optimise_disjunction(d: &mut Disjunction){
    remove_unused_alternatives(d);
    remove_duplicate_alternatives(d);
    optimise_alternatives(d);
}

pub fn remove_unused_alternatives(d: &mut Disjunction){
    let mut i = 0;
    let mut empty_alt = false;

    for alt in &mut d.alternatives{
        i += 1;
        if alt.terms.len() == 0{
            empty_alt = true;
            break;
        }
    };

    if empty_alt{
        d.alternatives.truncate(i);
    }
}

pub fn remove_duplicate_alternatives(d: &mut Disjunction){
    let mut need_remove = Vec::new();

    let l = d.alternatives.len() - 1;

    for i in 0..l{
        let a1 = &d.alternatives[i];
        let a2 = &d.alternatives[i + 1];

        if a1 == a2{
            need_remove.push(i + 1);
        }
    }

    let mut c:usize = 0;

    d.alternatives.retain(
        |alt|{
            if need_remove.contains(&c){
                c += 1;

                return false
            }
            c += 1;
            return true
        }
    );
}

pub fn optimise_alternatives(d: &mut Disjunction){
    for alt in &mut d.alternatives{
        //remove_duplicated_terms(alt);
    }
}

/*
pub fn remove_duplicated_terms(alt: &mut Alternative){
    let mut need_remove = Vec::new();

    let l = alt.terms.len() - 1;

    for i in 0..l{
        let t1 = &mut alt.terms[i..i+2];

        match &mut t1[0]{
            Term::Atom { atom: atom1, quantifier: quant1 } => {
                match &t1[1] {
                    Term::Atom { atom: atom2, quantifier:quant2 } => {
                        if atom1 == atom2{
                            need_remove.push(i + 1);

                            let q1 = quant1.as_ref().unwrap_or(
                                &Quantifier { 
                                    min: 1, 
                                    max: 1, 
                                    greedy: false
                                }
                            );

                            let q2 = quant2.as_ref().unwrap_or(
                                &Quantifier { 
                                    min: 1, 
                                    max: 1, 
                                    greedy: false
                                }
                            );

                            let q = Quantifier{
                                min: q1.min.checked_add(q2.min).unwrap_or(u32::MAX),
                                max: q1.max.checked_add(q2.max).unwrap_or(u32::MAX),
                                greedy: q1.greedy | q2.greedy
                            };

                            *quant1 = Some(q);
                        }
                    },
                    _ => {}
                }
            }
            _ => {}
        }
    };


    let mut c: usize = 0;

    alt.terms.retain(
        |term|{
            if need_remove.contains(&c){
                c += 1;
                return false;
            }
            c += 1;
            return true;
        }
    );
}

*/