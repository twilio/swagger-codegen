package dev.guardrail.generators.scala.zioHttp.server

object Helpers {

  val miniSpec: String =
    s"""
       |swagger: '2.0'
       |host: petstore.swagger.io
       |paths:
       |  "/store/orders":
       |    post:
       |      tags:
       |      - store
       |      x-jvm-package: store
       |      operationId: postOrderById
       |      produces:
       |      - application/xml
       |      - application/json
       |      parameters:
       |      - required: true
       |        name: body
       |        in: body
       |        schema:
       |          "$$ref": "#/definitions/Order"
       |      responses:
       |        '200':
       |          description: successful operation
       |          schema:
       |            "$$ref": "#/definitions/Order"
       |        '400':
       |          description: Invalid ID supplied
       |        '404':
       |          description: Order not found
       |definitions:
       |  Order:
       |    type: object
       |    properties:
       |      id:
       |        type: integer
       |        format: int64
       |      petId:
       |        type: integer
       |        format: int64
       |      quantity:
       |        type: integer
       |        format: int32
       |      shipDate:
       |        type: string
       |        format: date-time
       |      status:
       |        type: string
       |        description: Order Status
       |        enum:
       |        - placed
       |        - approved
       |        - delivered
       |      complete:
       |        type: boolean
       |        default: false
       |    xml:
       |      name: Order
       |  OrderStatus:
       |    type: string
       |    enum:
       |    - placed
       |    - approved
       |    - delivered
       |""".stripMargin

  val spec: String =
    s"""
       |swagger: '2.0'
       |host: petstore.swagger.io
       |paths:
       |  /:
       |    get:
       |      x-jvm-package: store
       |      operationId: getRoot
       |      responses:
       |        200:
       |          description: Successful
       |  "/store/order/{order_id}":
       |    get:
       |      tags:
       |      - store
       |      x-jvm-package: store
       |      operationId: getOrderById
       |      produces:
       |      - application/xml
       |      - application/json
       |      parameters:
       |      - name: order_id
       |        in: path
       |        required: true
       |        type: integer
       |        format: int64
       |      - name: status
       |        in: query
       |        required: true
       |        type: string
       |        x-scala-type: OrderStatus
       |        default: placed
       |      responses:
       |        '200':
       |          description: successful operation
       |          schema:
       |            "$$ref": "#/definitions/Order"
       |        '400':
       |          description: Invalid ID supplied
       |        '404':
       |          description: Order not found
       |  /foo/:
       |    get:
       |      x-jvm-package: store
       |      operationId: getFoo
       |      responses:
       |        200:
       |          schema:
       |            type: boolean
       |  "/foo/{bar}":
       |    get:
       |      x-jvm-package: store
       |      x-tracing-label: "completely-custom-label"
       |      operationId: getFooBar
       |      parameters:
       |      - name: bar
       |        in: path
       |        required: true
       |        type: integer
       |        format: int64
       |      responses:
       |        200:
       |          schema:
       |            type: boolean
       |  "/bar":
       |    put:
       |      operationId: putBar
       |      x-server-raw-response: true
       |      x-jvm-package: store
       |      parameters:
       |      - name: bar
       |        in: query
       |        required: true
       |        type: integer
       |        format: int64
       |      - required: true
       |        name: body
       |        in: body
       |        schema:
       |          "$$ref": "#/definitions/Order"
       |      responses:
       |        200:
       |          schema:
       |            type: boolean
       |definitions:
       |  Order:
       |    type: object
       |    properties:
       |      id:
       |        type: integer
       |        format: int64
       |      petId:
       |        type: integer
       |        format: int64
       |      quantity:
       |        type: integer
       |        format: int32
       |      shipDate:
       |        type: string
       |        format: date-time
       |      status:
       |        type: string
       |        description: Order Status
       |        enum:
       |        - placed
       |        - approved
       |        - delivered
       |      complete:
       |        type: boolean
       |        default: false
       |    xml:
       |      name: Order
       |  OrderStatus:
       |    type: string
       |    enum:
       |    - placed
       |    - approved
       |    - delivered
       |""".stripMargin

}
