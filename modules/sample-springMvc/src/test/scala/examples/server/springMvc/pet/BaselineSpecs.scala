package examples.server.springMvc.pet

import java.util
import java.util.concurrent.CompletableFuture

import examples.server.springMvc.definitions.{ ApiResponse, Pet }
import examples.server.springMvc.pet.PetHandler.{ FindPetsByStatusEnumResponse, FindPetsByStatusResponse, FindPetsByTagsResponse }
import org.junit.Assert.assertTrue
import org.junit.runner.RunWith
import org.mockito.{ ArgumentCaptor, ArgumentMatchersSugar, MockitoSugar }
import org.scalatest.BeforeAndAfterAll
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import org.springframework.beans.factory.annotation.Autowired
import org.springframework.boot.autoconfigure.EnableAutoConfiguration
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc
import org.springframework.boot.test.context.SpringBootTest
import org.springframework.context.annotation.ComponentScan
import org.springframework.http.MediaType
import org.springframework.mock.web.MockMultipartFile
import org.springframework.test.context.TestContextManager
import org.springframework.test.context.junit4.SpringRunner
import org.springframework.test.web.servlet.MockMvc
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders.{ asyncDispatch, get, post, put }
import org.springframework.test.web.servlet.result.MockMvcResultHandlers.print
import org.springframework.test.web.servlet.result.MockMvcResultMatchers.{ request, status }
import org.springframework.web.multipart.MultipartFile
import spring.test.TestApplication

import scala.jdk.CollectionConverters._

@RunWith(classOf[SpringRunner])
@SpringBootTest(classes = Array(classOf[TestApplication]))
@AutoConfigureMockMvc
@ComponentScan
@EnableAutoConfiguration
class BaselineSpecs extends AnyFreeSpec with Matchers with BeforeAndAfterAll with MockitoSugar with ArgumentMatchersSugar {
  @Autowired var mvc: MockMvc            = _
  @Autowired var handlerMock: PetHandler = _

  new TestContextManager(this.getClass).prepareTestInstance(this)

  override def beforeAll(): Unit =
    reset(handlerMock)

  "let's test pet-store generation" - {
    "query params" in {
      val entityBody = new util.ArrayList[Pet]()
      entityBody.add(new Pet.Builder("cat").build())
      entityBody.add(new Pet.Builder("mouse").build())

      when(handlerMock.findPetsByStatus(isA[java.util.List[String]]))
        .thenReturn(CompletableFuture.completedFuture(FindPetsByStatusResponse.Ok(entityBody)))

      val mvcResult = mvc
        .perform(
          get("/v2/pet/findByStatus?status=1")
            .accept(MediaType.APPLICATION_JSON)
        )
        .andExpect(request.asyncStarted)
        .andReturn

      mvc.perform(asyncDispatch(mvcResult)).andDo(print()).andExpect(status.isOk)

      val content = mvcResult.getResponse.getContentAsString
      assertTrue(content.contains("cat"))
      assertTrue(content.contains("mouse"))
    }

    "required param missing" in
      mvc
        .perform(
          get("/v2/pet/findByStatus")
            .accept(MediaType.APPLICATION_JSON)
        )
        .andExpect(status.is4xxClientError())

    "query params (list)" in {
      val entityBody = new util.ArrayList[Pet]()
      entityBody.add(new Pet.Builder("cat").build())

      when(handlerMock.findPetsByTags(List("blah", "foo", "bar").asJava))
        .thenReturn(CompletableFuture.completedFuture(FindPetsByTagsResponse.Ok(entityBody)))

      val mvcResult = mvc
        .perform(
          get("/v2/pet/findByTags?tags=blah&tags=foo&tags=bar")
            .accept(MediaType.APPLICATION_JSON)
        )
        .andExpect(request.asyncStarted)
        .andReturn

      mvc.perform(asyncDispatch(mvcResult)).andDo(print()).andExpect(status.isOk)

      val content = mvcResult.getResponse.getContentAsString
      assertTrue(content.contains("cat"))
    }

    "form params" in {

      when(handlerMock.updatePetWithForm(1L, java.util.Optional.of("Blah"), java.util.Optional.of("some_status")))
        .thenReturn(CompletableFuture.completedFuture(PetHandler.UpdatePetWithFormResponse.BadRequest))

      val mvcResult = mvc
        .perform(
          post("/v2/pet/1")
            .contentType(MediaType.APPLICATION_FORM_URLENCODED)
            .content("name=Blah&status=some_status")
        )
        .andExpect(request.asyncStarted)
        .andReturn

      mvcResult.getRequest.getPathInfo shouldBe "/v2/pet/1"

      mvc
        .perform(asyncDispatch(mvcResult))
        .andDo(print())
        .andExpect(status.isBadRequest)
    }

    "multipart" in {

      val firstFileCaptor  = ArgumentCaptor.forClass(classOf[java.util.Optional[MultipartFile]])
      val secondFileCaptor = ArgumentCaptor.forClass(classOf[MultipartFile])
      val thirdFileCaptor  = ArgumentCaptor.forClass(classOf[MultipartFile])

      when(
        handlerMock.uploadFile(
          any[java.lang.Long],
          any[java.util.Optional[String]],
          any[java.util.Optional[MultipartFile]],
          any[MultipartFile],
          any[MultipartFile],
          any[java.lang.Long],
          any[java.lang.Long],
          any[java.util.Optional[java.lang.Long]]
        )
      ).thenReturn(CompletableFuture.completedFuture(PetHandler.UploadFileResponse.Ok(new ApiResponse.Builder().build())))

      val secondFile = new MockMultipartFile(
        "file2",
        "other-file-name.data",
        "text/plain",
        ("some " +
          "other type").getBytes
      )
      val thirdFile = new MockMultipartFile(
        "file3",
        "other-file-name.data",
        "text/plain",
        ("some " +
          "other other type").getBytes
      )

      val mvcResult = mvc
        .perform(
          MockMvcRequestBuilders
            .multipart("/v2/pet/1/uploadImage")
            .file(secondFile)
            .file(thirdFile)
            .param("long-value", "4")
            .param("custom-value", "5")
        )
        .andExpect(request.asyncStarted)
        .andReturn

      mvc
        .perform(asyncDispatch(mvcResult))
        .andDo(print())
        .andExpect(status.isOk)

      verify(handlerMock).uploadFile(
        eqTo(1L),
        eqTo(java.util.Optional.empty()),
        firstFileCaptor.capture(),
        secondFileCaptor.capture(),
        thirdFileCaptor.capture(),
        eqTo(4L),
        eqTo(5L),
        eqTo(java.util.Optional.empty())
      )

      firstFileCaptor.getValue shouldBe (java.util.Optional.empty())
      secondFileCaptor.getValue.asInstanceOf[MultipartFile].getName shouldBe "file2"
      thirdFileCaptor.getValue.asInstanceOf[MultipartFile].getName shouldBe "file3"
    }

  }

  "body json object" in {
    when(handlerMock.updatePet(any[Pet]))
      .thenReturn(CompletableFuture.completedFuture(PetHandler.UpdatePetResponse.NotFound))

    val mvcResult = mvc
      .perform(
        put("/v2/pet")
          .contentType(MediaType.APPLICATION_JSON)
          .content("""{"name":"cat","id":2,"category":null,"photoUrls":[],"tags":null,"status":null}""")
      )
      .andExpect(request.asyncStarted)
      .andReturn

    mvcResult.getRequest.getPathInfo shouldBe "/v2/pet"

    mvc
      .perform(asyncDispatch(mvcResult))
      .andDo(print())
      .andExpect(status.isNotFound)

    val captor = ArgumentCaptor.forClass(classOf[Pet])
    verify(handlerMock).updatePet(captor.capture())

    captor.getValue.asInstanceOf[Pet].getName shouldBe "cat"
  }
}
