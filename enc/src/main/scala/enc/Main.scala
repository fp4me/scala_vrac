package enc

import java.security._
import java.util.Base64
import java.io._ //{File,FileInputStream, FileOutputStream}
import javax.crypto._
import javax.crypto.spec.IvParameterSpec
import javax.crypto.spec.SecretKeySpec
import javax.crypto.SecretKeyFactory
import javax.crypto.spec.PBEKeySpec


object CipherSample {

  private val TRANSFORMATION_AES = "AES/CBC/PKCS5Padding"
  private val password = "".toCharArray();
  private val salt = "".getBytes("UTF-8");

			//Note: initializationVector should be of length 16 bytes
	private val initializationVector = "".getBytes("UTF-8");


  def encryptor2(plainTextFile:File,encryptedLicenceFile:File) {

    if (encryptedLicenceFile.exists() == false) {
      try {
        encryptedLicenceFile.createNewFile();
      } catch {
        case e:IOException => throw new RuntimeException(e);
      }
    }
    else {
      try {
        encryptedLicenceFile.delete();
        encryptedLicenceFile.createNewFile();
      } catch {
        case e:IOException => throw new RuntimeException(e);
      }
    }

    val fileInputStream = new FileInputStream(plainTextFile);
    val fileOutputStream = new FileOutputStream(encryptedLicenceFile);

    try {

   SecureRandom.getInstance("SHA1PRNG").nextBytes(salt);


    val  keyFactory = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA1");
    val  keySpec = new PBEKeySpec(password, salt, 65536, 128);

    val secretKey = keyFactory.generateSecret(keySpec);
    val secret = new SecretKeySpec(secretKey.getEncoded(), "AES");

    val cipher = Cipher.getInstance("AES/CBC/PKCS5Padding");
    val ivParameterSpec = new IvParameterSpec(initializationVector);
    cipher.init(Cipher.ENCRYPT_MODE, secret, ivParameterSpec);

    val cipherInputStream = new CipherInputStream(fileInputStream, cipher);

    val buffer = new Array[Byte](4096)
    var read = 0;
    while ({read = cipherInputStream.read(buffer); read != -1}) {
      fileOutputStream.write(buffer,0,read);
    }

    fileOutputStream.flush();
   } catch {
     case e @ (
       _: InvalidAlgorithmParameterException |
       _: NoSuchPaddingException |
       _: NoSuchAlgorithmException |
       _: InvalidKeyException |
       _: IllegalBlockSizeException |
       _: FileNotFoundException |
       _: BadPaddingException) => throw new RuntimeException(e)
   } finally {
     try {
       if (fileInputStream != null) {
         fileInputStream.close();
       }

       if (fileOutputStream != null) {
         fileOutputStream.close();
       }
     } catch  {
       case e:IOException => throw new RuntimeException(e);
     }
   }
  }


  def decryptor2(encryptedFile:File,decryptedFile:File) {

    if (decryptedFile.exists() == false) {
      try {
        decryptedFile.createNewFile();
      } catch {
        case e:IOException => throw new RuntimeException(e);
      }
    }
    else {
      try {
        decryptedFile.delete();
        decryptedFile.createNewFile();
      } catch {
        case e:IOException => throw new RuntimeException(e);
      }
    }

    val fileInputStream = new FileInputStream(encryptedFile);

    val byteArrayOutputStream = new ByteArrayOutputStream();

    val fileOutputStream = new FileOutputStream(decryptedFile);

    try {

      val  keyFactory = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA1");
      val  keySpec = new PBEKeySpec(password, salt, 65536, 128);

      val secretKey = keyFactory.generateSecret(keySpec);
      val secret = new SecretKeySpec(secretKey.getEncoded(), "AES");

      val cipher = Cipher.getInstance("AES/CBC/PKCS5Padding");
      val ivParameterSpec = new IvParameterSpec(initializationVector);
      cipher.init(Cipher.DECRYPT_MODE, secret, ivParameterSpec);


    val cipherOutputStream = new CipherOutputStream(fileOutputStream, cipher);
    val buffer = new Array[Byte](4096) //new byte[4096];

    var read = 0;
    while ({read = fileInputStream.read(buffer); read != -1}) {
      cipherOutputStream.write(buffer,0,read);

    }

    if (cipherOutputStream != null) {
        //Unless you close here you won't get complete plain text
				cipherOutputStream.close();
			}

   } catch {
     case e @ (
       _: InvalidAlgorithmParameterException |
       _: NoSuchPaddingException |
       _: NoSuchAlgorithmException |
       _: InvalidKeyException |
       _: IllegalBlockSizeException |
       _: FileNotFoundException |
       _: BadPaddingException) => throw new RuntimeException(e)
   } finally {
     try {
       if (fileInputStream != null) {
         fileInputStream.close();
       }

       if (fileOutputStream != null) {
         fileOutputStream.close();
       }
     } catch  {
       case e:IOException => throw new RuntimeException(e);
     }
   }
  }

  def testEnc() : Unit = {
    val path= ""
    val pathEnc = path + ".enc"
    encryptor2(new File(path), new File(pathEnc));
  }

  def testDec() : Unit = {
    val path= ""
    val pathDec = path + ".dec"
    decryptor2(new File(path), new File(pathDec));
  }

  def test(): Unit = {
    val value = "Original Value"
    val keyAndIv = try {
      val keyGen = KeyGenerator.getInstance("AES")
      keyGen.init(128)
      //val secretKey = keyGen.generateKey()
      val secretKey =  new SecretKeySpec(Base64.getDecoder.decode("F5Z4RPmtqwBevzHIPA4HZQ=="),"AES");
      val salt = new Array[Byte](16)
      SecureRandom.getInstance("SHA1PRNG").nextBytes(salt)
      val iv = new IvParameterSpec(salt)
      (secretKey, iv)
    } catch {
      case e: NoSuchAlgorithmException => throw new RuntimeException(e);
    }
    val key = keyAndIv._1

    val bb= Base64.getEncoder.encodeToString(key.getEncoded);

    println(s"key:  $bb")
    val iv = keyAndIv._2
    println(s"1:  $value")
    val encryptedBytes = encrypt(value.getBytes, key, iv)
    println(s"2:  ${Base64.getEncoder.encodeToString(encryptedBytes)}")

    val key2 =  new SecretKeySpec(Base64.getDecoder.decode(bb),"AES");
    val decryptedBytes = decrypt(encryptedBytes, key2, iv)
    println(s"3:  ${new String(decryptedBytes)}")
  }

  def main(args: Array[String]): Unit = {
    testEnc();
    testDec();
  }

  private def encrypt(bytes: Array[Byte], key: Key, iv: IvParameterSpec) = {
    try {
      val cipher = Cipher.getInstance(TRANSFORMATION_AES)
      cipher.init(Cipher.ENCRYPT_MODE, key, iv)
      cipher.doFinal(bytes);
    } catch {
      case e @ (
        _: InvalidAlgorithmParameterException |
        _: NoSuchPaddingException |
        _: NoSuchAlgorithmException |
        _: InvalidKeyException |
        _: IllegalBlockSizeException |
        _: BadPaddingException) => throw new RuntimeException(e)
    }
  }

  private def decrypt(bytes: Array[Byte], key: Key, iv: IvParameterSpec) = {
    try {
      val cipher = Cipher.getInstance(TRANSFORMATION_AES)
      cipher.init(Cipher.DECRYPT_MODE, key, iv)
      cipher.doFinal(bytes);
    } catch {
      case e @ (
        _: InvalidAlgorithmParameterException |
        _: NoSuchPaddingException |
        _: NoSuchAlgorithmException |
        _: InvalidKeyException |
        _: IllegalBlockSizeException |
        _: BadPaddingException) => throw new RuntimeException(e)
      }
  }
}
