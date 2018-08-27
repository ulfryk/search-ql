import { DateExpression } from './date';
import { NumberExpression } from './number';
import { TermExpression } from './term';
import { TextExpression } from './text';

TermExpression.fromMatch = (match: string): TermExpression => {
  if (Number(match) === Number(match)) {
    return NumberExpression.fromMatch(match);
  }
  if (new Date(match).toJSON() != null) {
    return DateExpression.fromMatch(match);
  }
  return TextExpression.fromMatch(match);
};

export { DateExpression, NumberExpression, TermExpression, TextExpression };
export * from './selector';
