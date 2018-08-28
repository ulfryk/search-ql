import { TermExpression } from './term';

export class NumberExpression extends TermExpression<number> {

  public static fromMatch(match: string) {
    return new NumberExpression(match);
  }

  constructor(value: string) {
    super(value, Number(value.trim()));
  }

}
