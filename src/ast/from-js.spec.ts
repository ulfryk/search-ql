/* tslint:disable:no-unused-expression */
import { expect } from 'chai';

import { ExpressionType, OperatorType, ValueType } from '../common/model';
import { and, andNot, config, like, likeR, or, phrase, txt } from '../testing/utils';
import { fromJS } from './from-js';

describe('SearchQL ast', () => {

  describe('fromJS', () => {

    const ast = [
      txt('aaa AND ) a:aaaa OR OR OR'),
      and(phrase('aaa'), phrase('bbb')),
      likeR(txt('first_name'), txt('John')),
      like(txt('first_name'), txt('John')),
      andNot(phrase('aaa'), phrase('bbb')),
      or(phrase('aaa'), and(phrase('bbb'), phrase('ccc'))),
      or(phrase('aaa'), and(or(phrase('aaa'), and(phrase('bbb'), phrase('ccc'))), phrase('ccc'))),
      andNot(and(phrase('aaa'), phrase('bbb')), phrase('ccc')),
    ];

    ast.forEach(expression => {
      it(`should properly convert JSON created by 'toJS' method back to data (for ${expression})`,
        () => {
          expect(fromJS(config)(expression.toJS()).toJS()).to.deep.equal(expression.toJS());
          expect(fromJS(config)(expression.toJS()).equals(expression)).to.be.true;
        });
    });

    const pojo = {
      operator: {
        token: 'AND',
        type: OperatorType.And,
      },
      returnType: ValueType.Phrase,
      type: ExpressionType.Binary,
      value: [
        {
          operator: {
            token: '&',
            type: OperatorType.And,
          },
          returnType: ValueType.Phrase,
          type: ExpressionType.Binary,
          value: [
            {
              preparedValue: 'aaa',
              returnType: ValueType.Phrase,
              term: {
                preparedValue: 'aaa',
                returnType: ValueType.Text,
                type: ExpressionType.Text,
                value: 'aaa',
              },
              type: ExpressionType.Phrase,
              value: 'aaa',
            },
            {
              preparedValue: 'bbb',
              returnType: ValueType.Phrase,
              term: {
                preparedValue: 'bbb',
                returnType: ValueType.Text,
                type: ExpressionType.Text,
                value: 'bbb',
              },
              type: ExpressionType.Phrase,
              value: 'bbb',
            },
          ],
        },
        {
          returnType: ValueType.Phrase,
          type: ExpressionType.Not,
          value: {
            preparedValue: 'ccc',
            returnType: ValueType.Phrase,
            term: {
              preparedValue: 'ccc',
              returnType: ValueType.Text,
              type: ExpressionType.Text,
              value: 'ccc',
            },
            type: ExpressionType.Phrase,
            value: 'ccc',
          },
        },
      ],
    };

    it('should properly work for manual input', () => {
      expect(pojo).to.deep.equal(ast.slice().pop().toJS());
      expect(fromJS(config)(pojo).equals(ast.slice().pop())).to.be.true;
    });

  });

});
