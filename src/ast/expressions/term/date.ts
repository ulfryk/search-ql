import { TermExpression } from './term';

export class DateExpression extends TermExpression<number> {

  public static fromMatch(match: string) {
    return new DateExpression(match);
  }

  // TODO: probably use config here
  public static prepareValue(value: string) {
    return Date.parse(value.trim());
  }

  constructor(value: string) {
    super(value, DateExpression.prepareValue(value));
  }

}
