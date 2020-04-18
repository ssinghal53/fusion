/**
 * Copyright 2014, Sharad Singhal, All Rights Reserved
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 * 3. Neither the name of the copyright holder nor the names of its
 *    contributors may be used to endorse or promote products derived from
 *    this software without specific prior written permission.
 *    
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * 
 * Created June 28, 2014 by Sharad Singhal
 */

package net.aifusion.utils;

import java.io.InputStream;
import java.io.OutputStream;
import java.security.InvalidKeyException;
import java.security.Key;
import java.security.KeyFactory;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.PrivateKey;
import java.security.PublicKey;
import java.security.SecureRandom;
import java.security.Signature;
import java.security.SignatureException;
import java.security.spec.PKCS8EncodedKeySpec;
import java.security.spec.X509EncodedKeySpec;
import java.util.Arrays;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.crypto.Cipher;
import javax.crypto.CipherInputStream;
import javax.crypto.CipherOutputStream;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.KeyGenerator;
import javax.crypto.NoSuchPaddingException;
import javax.crypto.SecretKey;
import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.PBEKeySpec;
import javax.crypto.spec.SecretKeySpec;

import net.aifusion.metamodel.ExceptionReason;
import net.aifusion.metamodel.ModelException;

/**
 * Utility class to manage message encryption and signing
 * @author Sharad Singhal
 */
public class EncryptionUtility {
	private static Logger logger = Logger.getLogger(EncryptionUtility.class.getName());
	public static final int MIN_BUF_SIZE = 5000;
	/** Algorithm used to generate message signatures */
	public static final String signatureAlgorithm = "SHA512withRSA";
	/** Algorithm used to encrypt messages */
	public static final String symmetricKeyAlgorithm = "AES";
	/** Algorithm used to encrypt symmetric keys */
	public static final String encryptionAlgorithm = "RSA";
	/** Algorithm used for generating public keys for signing */
	public static final String publicKeyAlgorithm = "RSA";
	/** Encryption/Decryption mode used to encrypt messages with the symmetric key*/
	public static final String symmetricKeyCryptoMode = "AES/CBC/PKCS5Padding";
	/** Key length (in bits) of the symmetric keys */
	public static final int SYMMETRIC_KEY_LENGTH = 256;
	/** Key length (in bits) used for generating the wrapping keys */
	public static final int PUBLIC_KEY_LENGTH = 2048;
	/** Algorithm used to transform passwords+salts to secret keys */
	public static final String secretKeyGenerationAlgorithm = "PBKDF2WithHmacSHA1";
	/** Iteration count for generating secret key */
	public static final int SECRET_KEY_ITERATION = 8192;
	/** Default length of seeds */
	public static final int SEED_LENGTH = 16;
	/** Algorithm use to compute message digests */
	public static final String digestAlgorithm = "SHA-512";
	
	/**
	 * This class is not instantiated. All public methods are static.
	 */
	private EncryptionUtility(){
		return;		
	}
	
	/**
	 * Hash a pass phrase. A random seed and the passphrase are hashed. The seed is then
	 * prepended to the digest and returned as the hash
	 * @param passPhrase - passphrase to hash
	 * @return byte array containing the hash
	 * @see #validateHash(byte[], byte[])
	 */
	public static byte [] getHash(byte [] passPhrase){
		try {
			MessageDigest digest = MessageDigest.getInstance(digestAlgorithm);
			SecureRandom r = new SecureRandom();
			byte [] seed = new byte[SEED_LENGTH];
			r.nextBytes(seed);
			digest.update(seed);
			digest.update(passPhrase);
			byte [] d = digest.digest();
			byte [] hash = new byte[SEED_LENGTH+d.length];
			for(int i = 0; i < SEED_LENGTH; i++){
				hash[i] = seed[i];
			}
			for(int i = 0; i < d.length; i++){
				hash[i+SEED_LENGTH] = d[i];
			}
			return hash;	
		} catch (NoSuchAlgorithmException e) {
			// should not happen
			throw new ModelException("Internal Error - Unknown algorithm "+digestAlgorithm);
		}
	}
	
	/**
	 * Validate if a passPhrase matches a hash
	 * @param expectedHash - expected hash
	 * @param passPhrase - passphrase to test
	 * @return true if match, false otherwise
	 * @see #getHash(byte[])
	 */
	public static boolean validateHash(byte [] expectedHash, byte [] passPhrase){
		try {
			MessageDigest digest = MessageDigest.getInstance(digestAlgorithm);
			byte [] seed = Arrays.copyOfRange(expectedHash, 0, SEED_LENGTH);
			digest.update(seed);
			digest.update(passPhrase);
			byte [] d = digest.digest();
			if(d.length+SEED_LENGTH != expectedHash.length) return false;
			for(int i = 0; i < d.length; i++) {
				if(expectedHash[i+SEED_LENGTH] != d[i]) return false;
			}
			return true;
		} catch (NoSuchAlgorithmException e) {
			// should not happen
			throw new ModelException("Internal Error - Unknown algorithm "+EncryptionUtility.digestAlgorithm);
		}
	}

	/**
	 * Sign some plainText using a signature key
	 * @param signatureKey - sender's private signature key
	 * @param plainText - PlainText to sign
	 * @return - bytes containing the signature
	 * @see #verify(PublicKey, byte[], byte[])
	 */
	public static byte [] sign(PrivateKey signatureKey, byte [] plainText){
		try {
			Signature signer = Signature.getInstance(signatureAlgorithm);
			signer.initSign(signatureKey);
			signer.update(plainText);
			return signer.sign();
		} catch (SignatureException | NoSuchAlgorithmException | InvalidKeyException e) {
			// should not happen
			throw new ModelException("Unable to sign message "+new String(Arrays.copyOf(plainText, 10))+"...",e);
		}
	}

	/**
	 * Verify the signature on some signed plainText
	 * @param validationKey - sender's public validation key corresponding to the signature key
	 * @param plainText - signed plainText
	 * @param signature - signature received with the text
	 * @return true - if the signature is successfully verified, false otherwise
	 * @see #sign(PrivateKey, byte[])
	 */
	public static boolean verify(PublicKey validationKey, byte [] plainText, byte [] signature){
		try {
			Signature verifier = Signature.getInstance(signatureAlgorithm);
			verifier.initVerify(validationKey);
			verifier.update(plainText);
			return verifier.verify(signature);
		} catch(SignatureException | NoSuchAlgorithmException | InvalidKeyException e){
			logger.log(Level.WARNING,"Unable to verify incoming message "+new String(Arrays.copyOf(plainText, 10))+"...",e);
		}
		return false;
	}

	/**
	 * Wrap a secret key using a receiver's public encryption key
	 * @param key - key to wrap
	 * @param encryptionKey - receiver's public encryption key
	 * @return - wrapped key
	 */
	public static byte [] wrap(SecretKey key, PublicKey encryptionKey) {
		try {
			Cipher wrapper = Cipher.getInstance(encryptionAlgorithm);
			wrapper.init(Cipher.WRAP_MODE, encryptionKey);
			return wrapper.wrap(key);
		} catch (NoSuchAlgorithmException | NoSuchPaddingException | InvalidKeyException | IllegalBlockSizeException e) {
			// should not happen
			throw new ModelException("Internal Error - unable to wrap key",e);
		}
	}
	
	/**
	 * Unwrap a secret key using the receiver's private decryption key
	 * @param wrappedKey - wrapped key
	 * @param decryptionKey - receiver's private decryption key
	 * @return - unwrapped secret key
	 */
	public static SecretKey unwrap(byte [] wrappedKey, PrivateKey decryptionKey) {
		try {
			Cipher unwrapper = Cipher.getInstance(encryptionAlgorithm);
			unwrapper.init(Cipher.UNWRAP_MODE, decryptionKey);
			return (SecretKey) unwrapper.unwrap(wrappedKey,symmetricKeyAlgorithm,Cipher.SECRET_KEY);
		} catch (NoSuchAlgorithmException | NoSuchPaddingException | InvalidKeyException e) {
			throw new ModelException(ExceptionReason.INVALID_PARAMETER, "Unable to unwrap key "+new String(Arrays.copyOf(wrappedKey, 10))+"...",e);
		}
	}
	
	/**
	 * Encrypt some plaintext using a public encryption key
	 * @param plainText - plaintext to encrypt
	 * @param encryptionKey - public encryption key
	 * @return - encrypted text. Null in case of error
	 * @see #decrypt(byte[], PrivateKey)
	 */
	public static byte [] encrypt(byte [] plainText, PublicKey encryptionKey) {
		if(plainText == null) return null;
		// needed len = wrappedKey + E[ encodedVerificationKey + Signature + plainText ]
		byte [] outputBuffer = new byte[plainText.length+MIN_BUF_SIZE];
		try {
			// generate a secret symmetric key
			KeyGenerator keyGenerator = KeyGenerator.getInstance(symmetricKeyAlgorithm);
			keyGenerator.init(SYMMETRIC_KEY_LENGTH);
			SecretKey secretKey = keyGenerator.generateKey();
			// encrypt (wrap) the secret symmetric key using the public encryption key of the receiver
			Cipher wrapper = Cipher.getInstance(encryptionAlgorithm);
			wrapper.init(Cipher.WRAP_MODE, encryptionKey);
			byte [] wrappedSecretKey = wrapper.wrap(secretKey);

			// place the length of the wrappedKey followed by the wrapped key into the output buffer at outputBuffer[0]
			// cursor points to the next available location in outputBuffer
			int cursor = packToByteArray(wrappedSecretKey, outputBuffer, 0);
			int encryptionCursor = cursor;	// marker for start of encryption

			// add the length of the plain text to the message. We only push the length
			// of the message into the buffer, because we can do encryption in steps and avoid copies
			outputBuffer[cursor] = (byte)((plainText.length & 0xff00) >>> 8);	// msb of plainText
			outputBuffer[cursor+1] = (byte)(plainText.length & 0xff);			// lsb of plainText
			cursor += 2;

			// encrypt the message {PkLen,PublicKey,SLen,Signature,PtLen,PlainText}
			Cipher encryptor = Cipher.getInstance(symmetricKeyAlgorithm);
			encryptor.init(Cipher.ENCRYPT_MODE, secretKey);
			// encrypt upto the cursor, and reset cursor to point to the end of the encrypted stream in the output buffer
			cursor = encryptionCursor + encryptor.update(outputBuffer,encryptionCursor,cursor-encryptionCursor,outputBuffer,encryptionCursor);
			// encrypt the plain text. Cursor points to the end of the output buffer
			cursor += encryptor.doFinal(plainText, 0, plainText.length, outputBuffer, cursor);

			// return the encrypted bytes
			return Arrays.copyOfRange(outputBuffer, 0, cursor);
		} catch(Exception e){
			logger.log(Level.WARNING,"Unable to encrypt message "+new String(Arrays.copyOf(plainText, 10))+"...",e);
		}
		return null;
	}
	
	/**
	 * Decrypt some ciphertext using a private decryption key
	 * @param cipherText - ciphertext to decrypt
	 * @param decryptionKey - private decryption key
	 * @return - plain text. Null if error
	 * @see #encrypt(byte[], PublicKey)
	 */
	public static byte [] decrypt(byte [] cipherText, PrivateKey decryptionKey) {
		if(cipherText == null) return null;
		try {
			// note that we make a copy to avoid overwriting the received cipher text
			byte [] inputBuffer = Arrays.copyOf(cipherText,cipherText.length);

			// obtain the decryption key, and the cursor (current location in input buffer after the key)
			byte [] wrappedEncryptionKey = unpackFromByteArray(inputBuffer, 0);
			int cursor = wrappedEncryptionKey.length+2;
			// unwrap the enclosed symmetric key using our public encryption key
			Cipher unwrapper = Cipher.getInstance(encryptionAlgorithm);
			unwrapper.init(Cipher.UNWRAP_MODE, decryptionKey);
			Key k = unwrapper.unwrap(wrappedEncryptionKey,symmetricKeyAlgorithm,Cipher.SECRET_KEY);

			// decrypt the message.
			Cipher decryptor = Cipher.getInstance(symmetricKeyAlgorithm);
			decryptor.init(Cipher.DECRYPT_MODE, k);
			decryptor.doFinal(inputBuffer,cursor,inputBuffer.length-cursor,inputBuffer,cursor);
			
			// locate the plainText
			byte [] plainText = unpackFromByteArray(inputBuffer, cursor);
			// return the plain text
			return plainText;
		} catch(Exception e){
			logger.log(Level.WARNING,"Unable to decrypt cipherText "+cipherText, e);
		}
		return null;
	}

	/**
	 * Sign and encrypt a message for transmission to a receiver. The method uses the
	 * private signature key of the sender to sign the message, and the public encryption
	 * key of the receiver to encrypt the message contents.
	 * <p>An AES secret key is first generated, and encrypted (wrapped) using the receiver's
	 * public encryption key. The Sender's verificationKey, the messageSignature,
	 * and the plain text are encrypted using the secret key and the wrapped key and
	 * the encrypted text is returned packed as 
	 * {WLen,WrappedKey,E[{PkLen,PublicKey,SLen,Signature,PtLen,PlainText}]},
	 * where {x}Len is two bytes containing the length of {x}. Currently assumes that
	 * RSA keys are used for the asymetric keys
	 * @param senderKeys - keypair containing the sender's signature keys
	 * @param plainText - message to be sent (Should be less than about 60000 bytes).
	 * @param receiverEncryptionKey - the public (encryption) key of the receiver
	 * @return - encrypted message. Null in case of error
	 * @see #decryptAndVerify(PrivateKey, byte[])
	 */

	public static byte [] signAndEncrypt(KeyPair senderKeys, byte [] plainText, PublicKey receiverEncryptionKey){
		if(plainText == null) return null;
		// needed len = wrappedKey + E[ encodedVerificationKey + Signature + plainText ]
		byte [] outputBuffer = new byte[plainText.length+MIN_BUF_SIZE];
		try {
			// generate a secret symmetric key
			KeyGenerator keyGenerator = KeyGenerator.getInstance(symmetricKeyAlgorithm);
			keyGenerator.init(SYMMETRIC_KEY_LENGTH);
			SecretKey secretKey = keyGenerator.generateKey();
			// encrypt (wrap) the secret symmetric key using the public encryption key of the receiver
			Cipher wrapper = Cipher.getInstance(encryptionAlgorithm);
			wrapper.init(Cipher.WRAP_MODE, receiverEncryptionKey);
			byte [] wrappedSecretKey = wrapper.wrap(secretKey);

			// place the length of the wrappedKey followed by the wrapped key into the output buffer at outputBuffer[0]
			// cursor points to the next available location in outputBuffer
			int cursor = packToByteArray(wrappedSecretKey, outputBuffer, 0);
			int encryptionCursor = cursor;	// marker for start of encryption
			
			// pack the sender's public signature key into the output buffer
			cursor = packToByteArray(senderKeys.getPublic().getEncoded(), outputBuffer, cursor);

			// Compute the message signature and pack it. Note that the sign() method allows us to directly
			// place the signature in the outputBuffer, so we only insert the length of the signature rather
			// than first generating then copying the signature.
			Signature signer = Signature.getInstance(signatureAlgorithm);
			signer.initSign(senderKeys.getPrivate());
			signer.update(plainText);
			int len = signer.sign(outputBuffer, cursor+2, outputBuffer.length-cursor-2);
			outputBuffer[cursor] = (byte) ((len & 0xff00) >>> 8);	// msb of signature length
			outputBuffer[cursor+1] = (byte) (len & 0xff);			// msb of signature length
			cursor += len+2;

			// add the length of the plain text to the message. We only push the length
			// of the message into the buffer, because we can do encryption in steps and avoid copies
			outputBuffer[cursor] = (byte)((plainText.length & 0xff00) >>> 8);	// msb of plainText
			outputBuffer[cursor+1] = (byte)(plainText.length & 0xff);			// lsb of plainText
			cursor += 2;

			// encrypt the message {PkLen,PublicKey,SLen,Signature,PtLen,PlainText}
			Cipher encryptor = Cipher.getInstance(symmetricKeyAlgorithm);
			encryptor.init(Cipher.ENCRYPT_MODE, secretKey);
			// encrypt upto the cursor, and reset cursor to point to the end of the encrypted stream in the output buffer
			cursor = encryptionCursor + encryptor.update(outputBuffer,encryptionCursor,cursor-encryptionCursor,outputBuffer,encryptionCursor);
			// encrypt the plain text. Cursor points to the end of the output buffer
			cursor += encryptor.doFinal(plainText, 0, plainText.length, outputBuffer, cursor);

			// return the encrypted bytes
			return Arrays.copyOfRange(outputBuffer, 0, cursor);
		} catch(Exception e){
			logger.log(Level.WARNING,"Unable to encrypt or sign message "+new String(Arrays.copyOf(plainText, 10))+"...",e);
		}
		return null;
	}

	/**
	 * Decrypt a message, and verify the signature on it. The message
	 * is unpacked, the received encryption key is decrypted using the receiver's private
	 * decryption key, and the signature on the message is verified using the sender's 
	 * public key (included as part of the message). Currently assumes RSA key pairs
	 * @param decryptionKey - decryption key to be used
	 * @param receivedCipherText - received message
	 * @return - plainText. Null if some error occurs
	 * @see #signAndEncrypt(KeyPair, byte[], PublicKey)
	 */
	public static byte [] decryptAndVerify(PrivateKey decryptionKey, byte [] receivedCipherText){
		if(receivedCipherText == null) return null;
		try {
			// NOTE: If both signAndEncrypt() and decryptAndVeryfy() methods use byte [] as cipherText, then base64 conversion
			// can be avoided here
			// convert incoming message to bytes
			// byte [] inputBuffer = StringUtil.base64Decode(receivedCipherText);
			// note that we make a copy to avoid overwriting the received cipher text
			byte [] inputBuffer = Arrays.copyOf(receivedCipherText,receivedCipherText.length);

			// obtain the decryption key, and the cursor (current location in input buffer after the key)
			byte [] wrappedEncryptionKey = unpackFromByteArray(inputBuffer, 0);
			int cursor = wrappedEncryptionKey.length+2;
			// unwrap the enclosed symmetric key using our public encryption key
			Cipher unwrapper = Cipher.getInstance(encryptionAlgorithm);
			unwrapper.init(Cipher.UNWRAP_MODE, decryptionKey);
			Key k = unwrapper.unwrap(wrappedEncryptionKey,symmetricKeyAlgorithm,Cipher.SECRET_KEY);

			// decrypt the message. Note that decryption reduces the size of the message, so should fit
			// in the inputBuffer. We don't need the length of the decrypted message, since all lengths are
			// included in the message
			Cipher decryptor = Cipher.getInstance(symmetricKeyAlgorithm);
			decryptor.init(Cipher.DECRYPT_MODE, k);
			decryptor.doFinal(inputBuffer,cursor,inputBuffer.length-cursor,inputBuffer,cursor);

			// locate the sender's public key used for signatures and update cursor
			byte [] senderVerificationKey = unpackFromByteArray(inputBuffer, cursor);
			cursor += senderVerificationKey.length+2;

			// reconstruct the sender's public key used for message signing, and verify the signature
			// TODO: Note that the generation of the public key relies on the sender public key
			// using the algorithm defined by publicKeyAlgorithm. We need to encode the algorithm
			// as well
			X509EncodedKeySpec pubKeySpec = new X509EncodedKeySpec(senderVerificationKey);
			KeyFactory keyFactory = KeyFactory.getInstance(publicKeyAlgorithm);
			PublicKey pubKey = keyFactory.generatePublic(pubKeySpec);

			// locate the signature and update cursor
			// note: it is possible to reduce signature copy since verifier allows us to directly use the 
			// signature bits in inpuBuffer, but the size is small, so we leave this here for the moment
			byte [] signature = unpackFromByteArray(inputBuffer, cursor);
			cursor += signature.length+2;

			// locate the plainText
			byte [] plainText = unpackFromByteArray(inputBuffer, cursor);

			// verify the signature
			Signature verifier = Signature.getInstance(signatureAlgorithm);
			verifier.initVerify(pubKey);
			verifier.update(plainText);
			if(!verifier.verify(signature)){
				throw new ModelException("Unable to verify the message "+new String(Arrays.copyOf(plainText, 10)));
			}

			// return the plain text
			return plainText;
		} catch(Exception e){
			logger.log(Level.WARNING,"Unable to decrypt or verify incoming message "+receivedCipherText, e);
		}
		return null;
	}

	/**
	 * Pack a byte array into another byte array<br/>
	 * The input length followed by the bytes are packed into the data
	 * @param input - input array to be packed
	 * @param data - output buffer
	 * @param cursor - offset in the output buffer where the data needs to be packed
	 * @return - next available location in the data buffer
	 */
	private static int packToByteArray(byte [] input, byte [] data, int cursor) {
		int length = input.length;
		if(length > 65565){
			throw new RuntimeException("Input length exceeds 65565 bytes ["+length+"]");
		}
		if(cursor+length > data.length){
			throw new RuntimeException("Internal Error: Required space ["+cursor+length+"] exceeds available buffer size ["+data.length+"]");
		}
		data[cursor] =  (byte) ((length & 0xff00) >>> 8);	// MSB
		data[cursor+1] = (byte)(length & 0x00ff);			// LSB
		cursor += 2;
		for(int i = 0; i < length; i++){
			data[cursor+i] = input[i];
		}
		return cursor+length;
	}

	/**
	 * Unpack a byte array from another byte array<br/>
	 * The length of the array is read from the buffer, followed by the data
	 * @param data - input array
	 * @param cursor - starting cursor for the array to be read
	 * @return - byte array value
	 */
	private static byte [] unpackFromByteArray(byte[] data, int cursor) {
		if (cursor+1 >= data.length) {
			throw new ModelException("Internal Error: requested offset " + (cursor +1) + 
					" is beyond length of input array " + data.length);
		}
		int length = (data[cursor] & 0x00ff) << 8;	// MSB
		length |= data[cursor+1] & 0x00ff;			// LSB
		cursor += 2;
		if(cursor+length > data.length){
			throw new ModelException("Internal Error: Required space ["+cursor+length+"] exceeds data size ["+data.length+"]");
		}
		byte [] bytes = new byte[length];
		for(int i = 0; i <length; i++ ){
			bytes[i] = data[cursor+i];
		}
		return bytes;
	}
	
	/**
	 * Generate a private/public key pair usable by this class
	 * @return - a key pair containing public/private keys usable by this class
	 * @throws ModelException if the Java security policy does not permit keys of the appropriate length to be generated
	 * @see #publicKeyAlgorithm
	 * @see #PUBLIC_KEY_LENGTH
	 */
	public static KeyPair generatePublicKeyPair(){
		try {
			KeyPairGenerator keyGen = KeyPairGenerator.getInstance(publicKeyAlgorithm);
			keyGen.initialize(PUBLIC_KEY_LENGTH, new SecureRandom());
			return keyGen.generateKeyPair();
		} catch (Exception e) {
			// should not happen
			throw new ModelException("Internal key generation error - "+publicKeyAlgorithm+"["+PUBLIC_KEY_LENGTH+"]",e);
		}
	}
	
	/**
	 * Generate the private/public keys from encoded key data
	 * @param privateKeyData - private key data (from key.getEncoded())
	 * @param publicKeyData - public key data (from key.getEncoded()))
	 * @return - key pair containing the private/public keys
	 * @see java.security.Key#getEncoded()
	 * @see #publicKeyAlgorithm
	 */
	public static KeyPair generatePublicKeyPair(byte [] privateKeyData, byte [] publicKeyData){
		try {
			KeyFactory factory = KeyFactory.getInstance(publicKeyAlgorithm);
			// create the public key
			X509EncodedKeySpec pubKeySpec = new X509EncodedKeySpec(publicKeyData);
			PublicKey pubKey = factory.generatePublic(pubKeySpec);
			// create the private key
			PKCS8EncodedKeySpec privKeySpec = new PKCS8EncodedKeySpec(privateKeyData);
			PrivateKey privKey = factory.generatePrivate(privKeySpec);
			return new KeyPair(pubKey,privKey);
		} catch(Exception e){
			// should not happen unless corrupted data is passed
			throw new ModelException("Unable to generate key pair from encoded data",e);
		}
	}
	
	/**
	 * Generate a secret key
	 * @return - a secret key usable by this class
	 */
	public static SecretKey generateSecretKey() {
		// generate a secret symmetric key
		try {
			KeyGenerator keyGenerator = KeyGenerator.getInstance(symmetricKeyAlgorithm);
			keyGenerator.init(SYMMETRIC_KEY_LENGTH);
			return keyGenerator.generateKey();
		} catch (NoSuchAlgorithmException e) {
			// should not happen
			throw new ModelException("Internal Error-- unable to generate key",e);
		}
	}
	
	/**
	 * Generate a password-based secret key. This method takes a salt and a password, and generates an AES key from them
	 * @param salt - salt used in the algorithm (normally 16 bytes)
	 * @param password - password to be used to generate the secret key
	 * @return - key to be used in secret key encryption
	 * @see #encrypt(byte[], SecretKey)
	 * @see #decrypt(byte[], SecretKey)
	 * @see EncryptionUtility#secretKeyGenerationAlgorithm
	 * @see #symmetricKeyAlgorithm
	 * @see #SYMMETRIC_KEY_LENGTH
	 */
	public static SecretKey generateSecretKey(byte [] salt, char [] password) {
		try {
			// create a PBE key specification
			PBEKeySpec spec = new PBEKeySpec(password, salt, SECRET_KEY_ITERATION, SYMMETRIC_KEY_LENGTH);	
			// get a PBE factory class
			SecretKeyFactory factory = SecretKeyFactory.getInstance(secretKeyGenerationAlgorithm);
			// get a Password based key to use as key material
			byte [] keyMaterial = factory.generateSecret(spec).getEncoded();
			// get an AES key from the material
			return new SecretKeySpec(keyMaterial, symmetricKeyAlgorithm);
		} catch (Exception e) {
			// should not happen
			throw new ModelException("Internal error generating secret key",e);
		}
	}
	
	/**
	 * Encrypt some plain text using secret key-based encryption
	 * @param plainText - plainText to be encrypted
	 * @param key - secret key for encryption
	 * @return - encrypted cipher text
	 * @see #generateSecretKey(byte[], char[])
	 * @see #decrypt(byte[], SecretKey)
	 */
	public static byte [] encrypt(byte [] plainText, SecretKey key){
		try {
			Cipher cipher = Cipher.getInstance(symmetricKeyCryptoMode);
			cipher.init(Cipher.ENCRYPT_MODE, key);
			byte [] initVector = cipher.getIV();
			byte [] outputBuffer = new byte [cipher.getOutputSize(plainText.length)+initVector.length+2];
			int cursor = packToByteArray(initVector, outputBuffer, 0);
			cipher.doFinal(plainText, 0, plainText.length, outputBuffer,cursor);
			return outputBuffer;
		} catch (Exception e){
			throw new ModelException("Error encrypting plainText "+plainText,e);
		}
	}
	
	/**
	 * Decrypt some cipherText that was encrypted using secret key-based encryption
	 * @param cipherText - cipherText to be decrypted
	 * @param key - secret key to be used for decryption
	 * @return - decrypted plain text
	 * @see #generateSecretKey(byte[], char[])
	 * @see #encrypt(byte[], SecretKey)
	 */
	public static byte [] decrypt(byte [] cipherText, SecretKey key){
		try {
			byte [] initVector = unpackFromByteArray(cipherText, 0);
			int cursor = initVector.length+2;			
			Cipher cipher = Cipher.getInstance(symmetricKeyCryptoMode);
			cipher.init(Cipher.DECRYPT_MODE, key, new IvParameterSpec(initVector));			
			return cipher.doFinal(cipherText,cursor,cipherText.length-cursor);
		} catch (Exception e){
			throw new ModelException(ExceptionReason.INVALID_PARAMETER, "Error decrypting cipherText "+cipherText,e);
		}
	}
		
	/**
	 * Encrypt some plain text using a password. An encryption key is generated using the password 
	 * and a randomly generated salt, and used to encrypt the message. The
	 * salt concatenated with the encoded message is returned as the cipherText
	 * @param plainText - plain text to be encrypted
	 * @param password - password to be used for encryption
	 * @return - encrypted cipherText
	 * @see #decrypt(byte[], char[])
	 */
	public static byte [] encrypt(byte [] plainText, char [] password){
		// create a secret key using a random salt
		byte [] salt = new byte[SEED_LENGTH];
		SecureRandom random = new SecureRandom();
		random.nextBytes(salt);
		SecretKey key = generateSecretKey(salt, password);
		try {
			Cipher cipher = Cipher.getInstance(symmetricKeyCryptoMode);
			cipher.init(Cipher.ENCRYPT_MODE, key, new IvParameterSpec(salt));
			byte [] encoded =  cipher.doFinal(plainText);
			byte [] cipherText = new byte[encoded.length+salt.length+4];
			int cursor = 0;
			// pack the salt dataValue and the encoded values to create the cipher text
			cursor = packToByteArray(salt, cipherText, cursor);
			packToByteArray(encoded,cipherText,cursor);
			return cipherText;
		} catch (Exception e){
			throw new ModelException("Error encrypting plainText "+plainText,e);
		}
	}

	/**
	 * Decrypt some cipherText that was encrypted using password-based encryption. The salt is extracted
	 * from the message, and the password is used to re-generate the encryption key, which is then used
	 * to decrypt the message.
	 * @param cipherText - cipherText to be decrypted
	 * @param password - password used to encrypt the text
	 * @return - decrypted plain text
	 * @see #encrypt(byte[], char[])
	 */
	public static byte [] decrypt(byte [] cipherText, char [] password){
		int cursor = 0;
		byte [] salt = unpackFromByteArray(cipherText,cursor);
		SecretKey key = generateSecretKey(salt, password);
		cursor += salt.length+2;
		byte [] encoded = unpackFromByteArray(cipherText,cursor);
		try {
			Cipher cipher = Cipher.getInstance(symmetricKeyCryptoMode);
			cipher.init(Cipher.DECRYPT_MODE, key, new IvParameterSpec(salt) );
			return cipher.doFinal(encoded);
		} catch (Exception e){
			throw new ModelException("Error decrypting cipherText "+cipherText,e);
		}
	}
	
	public static void encrypt(InputStream input, CipherOutputStream output, SecretKey key) {
		throw new ModelException("Not yet implemented");
	}
	
	public static void decrypt(CipherInputStream input, OutputStream output, SecretKey key) {
		throw new ModelException("Not yet implemented");
	}
	
}
