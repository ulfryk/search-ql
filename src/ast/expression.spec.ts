/* tslint:disable:no-unused-expression */
import { expect } from 'chai';
import { Map } from 'immutable';

import { Expression, ExpressionType, OperatorType, ValueType } from '../common/model';
import { InvalidExpression } from './expressions';
import { fromJS } from './from-js';

const getErrors = (e: Expression) => e.toList()
  .filter(node => node instanceof InvalidExpression)
  .flatMap(expr => (expr as InvalidExpression).errors.map(String))
  .toArray();

describe('SearchQL ast: Expression tree', () => {

  xit('should be able to self check types', () => {
    expect(Expression).to.equal(Expression);
  });

  describe('integrity', () => {

    const model = Map<string, ValueType>({
      first_name: ValueType.Text,
    });

    const validDTO = [
      {
        returnType: ValueType.Text,
        type: ExpressionType.Text,
        value: 'ccc',
      },
      {
        returnType: ValueType.Number,
        type: ExpressionType.Number,
        value: '00123.00',
      },
      {
        returnType: ValueType.Date,
        timeFrame: {
          end: 1533195898070,
          start: 1533195898070,
          text: '2018-08-02T07:44:58.070Z',
        },
        type: ExpressionType.Date,
        value: '2018-08-02T07:44:58.070Z',
      },
      {
        matchingType: ValueType.Text,
        returnType: ValueType.Text,
        type: ExpressionType.Selector,
        value: 'first_name',
      },
      {
        returnType: ValueType.Phrase,
        term: {
          returnType: ValueType.Text,
          type: ExpressionType.Text,
          value: ' lorem ipsum ',
        },
        type: ExpressionType.Phrase,
        value: ' lorem ipsum ',
      },

      {
        operator: {
          token: '&',
          type: OperatorType.And,
        },
        returnType: ValueType.Phrase,
        type: ExpressionType.Binary,
        value: [
          {
            returnType: ValueType.Phrase,
            term: {
              returnType: ValueType.Text,
              type: ExpressionType.Text,
              value: 'aaa',
            },
            type: ExpressionType.Phrase,
            value: 'aaa',
          },
          {
            returnType: ValueType.Phrase,
            term: {
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
        operator: {
          token: '<',
          type: OperatorType.Lt,
        },
        returnType: ValueType.Boolean,
        type: ExpressionType.Binary,
        value: [
          {
            returnType: ValueType.Number,
            type: ExpressionType.Number,
            value: '00012',
          },
          {
            returnType: ValueType.Number,
            type: ExpressionType.Number,
            value: '13.00',
          },
        ],
      },
    ];

    const invalidDTO = [

      // --- NUMBER ---
      [{
        returnType: ValueType.Number,
        type: ExpressionType.Number,
        value: 'xxe0012300',
      }, [
        'IntegrityError: NumberExpression contains a non-number value: "xxe0012300".',
      ]],

      // --- DATE ---
      [{
        returnType: ValueType.Date,
        timeFrame: {
          end: 0,
          start: 0,
          text: 'asd2018-08-02asasd',
        },
        type: ExpressionType.Date,
        value: 'asd2018-08-02asasd',
      }, [
        'IntegrityError: DateExpression contains a non-date value: "asd2018-08-02asasd".',
      ]],

      // --- SELECTOR ---
      [{
        matchingType: ValueType.Number,
        returnType: ValueType.Text,
        type: ExpressionType.Selector,
        value: 'first_name',
      }, ['IntegrityError: SelectorExpression is invalid - "matchingType" should equal TEXT but is NUMBER']],
      [{
        matchingType: ValueType.Text,
        returnType: ValueType.Text,
        type: ExpressionType.Selector,
        value: 'firstName',
      }, ['IntegrityError: SelectorExpression is invalid - "firstName" is not available on model definition']],

      // --- BINARY ---
      [{
        operator: {
          token: '>=',
          type: OperatorType.Gte,
        },
        returnType: ValueType.Boolean,
        type: ExpressionType.Binary,
        value: [
          {
            returnType: ValueType.Phrase,
            term: {
              returnType: ValueType.Text,
              type: ExpressionType.Text,
              value: 'aaa',
            },
            type: ExpressionType.Phrase,
            value: 'aaa',
          },
          {
            returnType: ValueType.Phrase,
            term: {
              returnType: ValueType.Text,
              type: ExpressionType.Text,
              value: 'bbb',
            },
            type: ExpressionType.Phrase,
            value: 'bbb',
          },
        ],
      }, [
        'IntegrityError: The LHS of >= shouldn\'t evaluate to Phrase.',
        'IntegrityError: The RHS of >= shouldn\'t evaluate to Phrase.',
      ]],

    ] as [any, string[]][];

    validDTO.forEach(dto => {
      const ast = fromJS(dto);
      const integrityChecked = ast.checkIntegrity(model);
      const integrityTypesChecked = ast.checkIntegrity(model).checkTypes();
      const typesIntegrityChecked = ast.checkTypes().checkIntegrity(model);

      it(`should be able to self check integrity (valid expression: ${ast})`, () => {
        expect(ast).to.be.instanceOf(Expression);
        expect(integrityChecked.isValid(), getErrors(integrityChecked).join(';')).to.be.true;
        expect(integrityTypesChecked.isValid(), getErrors(integrityTypesChecked).join(';')).to.be.true;
        expect(typesIntegrityChecked.isValid(), getErrors(typesIntegrityChecked).join(';')).to.be.true;
      });
    });

    invalidDTO.forEach(([dto, errors]) => {
      const ast = fromJS(dto);
      const integrityChecked = ast.checkIntegrity(model);
      const integrityTypesChecked = ast.checkIntegrity(model).checkTypes();
      const typesIntegrityChecked = ast.checkTypes().checkIntegrity(model);

      it(`should be able to self check integrity (invalid expression: ${ast})`, () => {
        expect(ast).to.be.instanceOf(Expression);
        expect(integrityChecked.isValid()).to.be.false;
        expect(integrityTypesChecked.isValid()).to.be.false;
        expect(typesIntegrityChecked.isValid()).to.be.false;

        expect(getErrors(integrityChecked)).to.deep.equal(getErrors(typesIntegrityChecked));
        expect(getErrors(integrityChecked)).to.deep.equal(getErrors(integrityTypesChecked));
        expect(getErrors(integrityChecked)).to.deep.equal(errors);
      });
    });

  });

});
