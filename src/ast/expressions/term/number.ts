import { TermExpression } from './term';

export class NumberExpression extends TermExpression<number> {

  public static fromMatch(match: string) {
    return new NumberExpression(match);
  }

  // TODO: probably use config here
  public static prepareValue(value: string) {
    return Number(value.trim());
  }

  constructor(value: string) {
    super(value, NumberExpression.prepareValue(value));
  }

}
