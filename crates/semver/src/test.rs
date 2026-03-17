use super::Comparator;
use super::ComparatorKind;
use super::{Range, RangeSet, Version};

pub(super) fn test_disjunction(version: &Version, alternatives: &RangeSet) -> bool {
    if alternatives.ranges.is_empty() {
        return true;
    }
    for alternative in &alternatives.ranges {
        if test_alternative(version, alternative) {
            return true;
        }
    }

    false
}

fn test_alternative(version: &Version, alternative: &Range) -> bool {
    match alternative {
        Range::Hyphen(hyphen_range) => todo!(),
        Range::Comparators(comparators) => {
            for comparator in comparators {
                if !test_comparator(version, comparator) {
                    return false;
                }
            }
            true
        }
        Range::Any => true,
    }
}

fn test_comparator(version: &Version, comparator: &Comparator) -> bool {
    let cmp = version.compare_to(&comparator.version);
    let cmp = cmp as i8;
    match comparator.kind {
        ComparatorKind::Exact => cmp == 0,
        ComparatorKind::Greater => cmp > 0,
        ComparatorKind::GreaterEq => cmp >= 0,
        ComparatorKind::Less => cmp < 0,
        ComparatorKind::LessEq => cmp <= 0,
        ComparatorKind::Eq => cmp == 0,
        ComparatorKind::Tilde => todo!(),
        ComparatorKind::Caret => todo!(),
    }
}
