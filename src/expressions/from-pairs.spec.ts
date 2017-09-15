/* tslint:disable:no-unused-expression */
import { expect } from 'chai';
import { Set } from 'immutable';
import * as _ from 'lodash';
import { Maybe, None, Some } from 'monet';

import { AND, LogicOperator, OR } from '../syntax-config';
import { BasicExpression } from './basic-expression';
import { Expression } from './expression';
import { fromPairs } from './from-pairs';
import { JoinedExpression } from './joined-expression';

describe('SearchQL expressions', () => {

  describe('fromPairs', () => {

    const validInput = [
      [[None<LogicOperator>(), new BasicExpression('aaa AND ) a:aaaa OR OR OR')]],
      [
        [None<LogicOperator>(), new BasicExpression('aaa')],
        [Some(AND), new BasicExpression('bbb')],
      ],
      [
        [None<LogicOperator>(), new BasicExpression('bbb')],
        [Some(AND), new BasicExpression('bbb')],
        [Some(AND), new BasicExpression('bbb')],
      ],
      [
        [None<LogicOperator>(), new BasicExpression('aaa')],
        [Some(OR), new BasicExpression('bbb')],
        [Some(AND), new BasicExpression('ccc')],
      ],
      [
        [None<LogicOperator>(), new BasicExpression('bbb')],
        [Some(AND), new BasicExpression('ccc')],
        [Some(AND), new BasicExpression('ddd')],
      ],
      [
        [None<LogicOperator>(), new BasicExpression('aaa')],
        [Some(OR), fromPairs([
          [None<LogicOperator>(), new BasicExpression('aaa')] as [Maybe<LogicOperator>, Expression],
          [Some(OR), new BasicExpression('bbb')],
          [Some(AND), new BasicExpression('ccc')],
        ] as [Maybe<LogicOperator>, Expression][])],
        [Some(AND), new BasicExpression('ccc')],
      ],
    ];

    const validOutput = [
      new BasicExpression('aaa AND ) a:aaaa OR OR OR'),
      new JoinedExpression(AND, Set([
        new BasicExpression('aaa'),
        new BasicExpression('bbb'),
      ])),
      new JoinedExpression(AND, Set([
        new BasicExpression('bbb'),
      ])),
      new JoinedExpression(AND, Set([
        new JoinedExpression(OR, Set([
          new BasicExpression('aaa'),
          new BasicExpression('bbb'),
        ])),
        new BasicExpression('ccc'),
      ])),
      new JoinedExpression(AND, Set([
        new BasicExpression('bbb'),
        new BasicExpression('ccc'),
        new BasicExpression('ddd'),
      ])),
      new JoinedExpression(AND, Set([
        new JoinedExpression(OR, Set([
          new BasicExpression('aaa'),
          new JoinedExpression(AND, Set([
            new JoinedExpression(OR, Set([
              new BasicExpression('aaa'),
              new BasicExpression('bbb'),
            ])),
            new BasicExpression('ccc'),
          ])),
        ])),
        new BasicExpression('ccc'),
      ])),
    ];

    _.zip<any>(validInput, validOutput).forEach(([input, output]) => {
      it(`should properly build expression for: ${output}`, () => {
        expect(fromPairs(input).equals(output)).to.be.true;
      });
    });

  });

});
