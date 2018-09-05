import { ValueType } from '../../../common/model';
import { isDate } from './is-date';
import { TermExpression } from './term';

export class DateExpression extends TermExpression<number> {

  public static fromTerm(term: TermExpression) {
    return isDate(term.value) ? DateExpression.of(term.value) : term;
  }

  public static of(value: string) {
    return new DateExpression(value);
  }

  // TODO: probably use config here
  public static prepareValue(value: string): number {
    return Date.parse(value.trim());
  }

  public readonly returnType = ValueType.Date;

  constructor(value: string) {
    super(value, DateExpression.prepareValue(value));
  }

}
