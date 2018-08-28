import { TermExpression } from './term';

export class DateExpression extends TermExpression<number> {

  public static fromMatch(match: string) {
    return new DateExpression(match);
  }

  constructor(value: string) {
    super(value, Date.parse(value.trim()));
  }

}
