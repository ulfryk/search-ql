import { ValueType } from '../../../common/model';
import { TermExpression } from './term';

export class PhraseExpression extends TermExpression<string> {

  public static fromTerm({ value }: TermExpression) {
    return PhraseExpression.of(value);
  }

  public static of(value: string) {
    return new PhraseExpression(value);
  }

  public readonly returnType: ValueType = ValueType.Phrase;

  constructor(value: string) {
    super(value, value.trim());
  }

}

// TBD:

// import { ValueType } from '../../../common/model';
// import { fromMatch } from './from-match';
// import { TermExpression } from './term';

// export class PhraseExpression<T> extends TermExpression<T> {

//   public static fromTerm<G, C extends TermExpression<G>>(term: C): PhraseExpression<G> {
//     return new PhraseExpression(term);
//   }

//   public static of<G>(value: string) {
//     return new PhraseExpression<G>(fromMatch(value));
//   }

//   public readonly returnType: ValueType = ValueType.Phrase;

//   constructor(public readonly term: TermExpression<T>) {
//     super(term.value, term.preparedValue);
//   }

// }
