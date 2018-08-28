/* tslint:disable:no-unused-expression */
import { expect } from 'chai';
import { zip } from 'lodash';
import { Maybe, None, Some } from 'monet';

import { SyntaxConfig } from '../config';
import { BinaryOperationExpression, Expression, NotExpression, SelectorExpression, TextExpression } from './expressions';
import { fromPairs } from './from-pairs';
import { AndOperator, LikeOperator, OrOperator } from './operators';

const config = new SyntaxConfig();
const { AND, LIKE, NOT, OR } = config;
const And = new AndOperator(AND);
const Like = new LikeOperator(LIKE);
const Or = new OrOperator(OR);

describe('SearchQL expressions', () => {

  describe('fromPairs', () => {

    const validInput = [
      [[None<string>(), new TextExpression('aaa AND ) a:aaaa OR OR OR')]],
      [
        [None<string>(), new TextExpression('aaa')],
        [Some(AND), new TextExpression('bbb')],
      ],
      [
        [None<string>(), new TextExpression('first_name')],
        [Some(LIKE), new TextExpression('John')],
      ],
      [
        [None<string>(), new TextExpression('aaa')],
        [Some(NOT), new TextExpression('bbb')],
      ],
      [
        [None<string>(), new TextExpression('aaa')],
        [Some(OR), new TextExpression('bbb')],
        [Some(AND), new TextExpression('ccc')],
      ],
      [
        [None<string>(), new TextExpression('aaa')],
        [Some(OR), fromPairs([
          [None<string>(), new TextExpression('aaa')],
          [Some(OR), new TextExpression('bbb')],
          [Some(AND), new TextExpression('ccc')],
        ] as [Maybe<string>, Expression][], config)],
        [Some(AND), new TextExpression('ccc')],
      ],
      [
        [None<string>(), new BinaryOperationExpression(And, [
          new TextExpression('aaa'),
          new TextExpression('bbb'),
        ])],
        [Some(NOT), new TextExpression('ccc')],
      ],
    ];

    const validOutput = [
      new TextExpression('aaa AND ) a:aaaa OR OR OR'),
      new BinaryOperationExpression(And, [
        new TextExpression('aaa'),
        new TextExpression('bbb'),
      ]),
      new BinaryOperationExpression(Like, [
        new SelectorExpression('first_name'),
        new TextExpression('John'),
      ]),
      new BinaryOperationExpression(And, [
        new TextExpression('aaa'),
        new NotExpression(new TextExpression('bbb')),
      ]),
      new BinaryOperationExpression(And, [
        new BinaryOperationExpression(Or, [
          new TextExpression('aaa'),
          new TextExpression('bbb'),
        ]),
        new TextExpression('ccc'),
      ]),
      new BinaryOperationExpression(And, [
        new BinaryOperationExpression(Or, [
          new TextExpression('aaa'),
          new BinaryOperationExpression(And, [
            new BinaryOperationExpression(Or, [
              new TextExpression('aaa'),
              new TextExpression('bbb'),
            ]),
            new TextExpression('ccc'),
          ]),
        ]),
        new TextExpression('ccc'),
      ]),
      new BinaryOperationExpression(And, [
        new BinaryOperationExpression(And, [
          new TextExpression('aaa'),
          new TextExpression('bbb'),
        ]),
        new NotExpression(new TextExpression('ccc')),
      ]),
    ];

    zip<any>(validInput, validOutput).forEach(([input, output]) => {
      it(`should properly build expression for: ${output}`, () => {
        expect(fromPairs(input, config).equals(output)).to.be.true;
      });
    });

  });

});
