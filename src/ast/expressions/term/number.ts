import { TermExpression } from './term';

export class NumberExpression extends TermExpression {

  public static fromMatch(match: string) {
    return new NumberExpression(match);
  }

}
