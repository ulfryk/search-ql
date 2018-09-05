import { ValueType } from '../../../common/model';
import { isNumber } from './is-number';
import { TermExpression } from './term';

export class NumberExpression extends TermExpression<number> {

  public static fromTerm(term: TermExpression) {
    return isNumber(term.value) ? NumberExpression.of(term.value) : term;
  }

  public static of(value: string) {
    return new NumberExpression(value);
  }

  // TODO: probably use config here
  public static prepareValue(value: string): number {
    return Number(value.trim());
  }

  public readonly returnType = ValueType.Number;

  constructor(value: string) {
    super(value, NumberExpression.prepareValue(value));
  }

}
