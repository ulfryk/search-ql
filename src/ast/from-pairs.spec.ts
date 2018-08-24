/* tslint:disable:no-unused-expression */
import { expect } from 'chai';
import { Set } from 'immutable';
import { zip } from 'lodash';
import { Maybe, None, Some } from 'monet';

import { SyntaxConfig } from '../syntax-config';
import { BasicExpression } from './basic-expression';
import { Expression } from './expression';
import { fromPairs } from './from-pairs';
import { JoinedExpression } from './joined-expression';
import { NotExpression } from './not-expression';

const config = new SyntaxConfig();
const { AND, NOT, OR } = config;

describe('SearchQL expressions', () => {

  describe('fromPairs', () => {

    const validInput = [
      [[None<string>(), new BasicExpression('aaa AND ) a:aaaa OR OR OR')]],
      [
        [None<string>(), new BasicExpression('aaa')],
        [Some(AND), new BasicExpression('bbb')],
      ],
      [
        [None<string>(), new BasicExpression('aaa')],
        [Some(NOT), new BasicExpression('bbb')],
      ],
      [
        [None<string>(), new BasicExpression('bbb')],
        [Some(AND), new BasicExpression('bbb')],
        [Some(AND), new BasicExpression('bbb')],
      ],
      [
        [None<string>(), new BasicExpression('aaa')],
        [Some(OR), new BasicExpression('bbb')],
        [Some(AND), new BasicExpression('ccc')],
      ],
      [
        [None<string>(), new BasicExpression('bbb')],
        [Some(AND), new BasicExpression('ccc')],
        [Some(AND), new BasicExpression('ddd')],
      ],
      [
        [None<string>(), new BasicExpression('aaa')],
        [Some(OR), fromPairs([
          [None<string>(), new BasicExpression('aaa')],
          [Some(OR), new BasicExpression('bbb')],
          [Some(AND), new BasicExpression('ccc')],
        ] as [Maybe<string>, Expression][], config)],
        [Some(AND), new BasicExpression('ccc')],
      ],
      [
        [None<string>(), new JoinedExpression(AND, Set([
          new BasicExpression('aaa'),
          new BasicExpression('bbb'),
        ]))],
        [Some(NOT), new BasicExpression('ccc')],
      ],
    ];

    const validOutput = [
      new BasicExpression('aaa AND ) a:aaaa OR OR OR'),
      new JoinedExpression(AND, Set([
        new BasicExpression('aaa'),
        new BasicExpression('bbb'),
      ])),
      new JoinedExpression(AND, Set([
        new BasicExpression('aaa'),
        new NotExpression(new BasicExpression('bbb')),
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
      new JoinedExpression(AND, Set([
        new JoinedExpression(AND, Set([
          new BasicExpression('aaa'),
          new BasicExpression('bbb'),
        ])),
        new NotExpression(new BasicExpression('ccc')),
      ])),
    ];

    zip<any>(validInput, validOutput).forEach(([input, output]) => {
      it(`should properly build expression for: ${output}`, () => {
        expect(fromPairs(input, config).equals(output)).to.be.true;
      });
    });

  });

});
