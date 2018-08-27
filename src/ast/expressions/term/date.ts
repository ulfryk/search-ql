import { TermExpression } from './term';

export class DateExpression extends TermExpression {

  public static fromMatch(match: string) {
    return new DateExpression(match);
  }

}
