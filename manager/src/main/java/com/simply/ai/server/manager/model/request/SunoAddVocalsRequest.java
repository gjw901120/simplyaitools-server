package com.simply.ai.server.manager.model.request;

import com.simply.ai.server.manager.enums.SunoModelEnum;
import lombok.Data;
import lombok.EqualsAndHashCode;
import org.hibernate.validator.constraints.URL;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.Size;
import java.io.Serializable;

/**
 * 添加人声生成音乐请求参数
 */
@Data
@EqualsAndHashCode(callSuper = true)
public class SunoAddVocalsRequest extends SunoBaseRequest implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 提示词
     */
    @NotBlank(message = "提示词不能为空")
    @Size(max = 5000, message = "提示词长度不能超过5000个字符")
    private String prompt;

    /**
     * 音乐标题
     */
    @NotBlank(message = "标题不能为空")
    @Size(max = 100, message = "标题长度不能超过100个字符")
    private String title;

    /**
     * 排除的音乐风格
     */
    @NotBlank(message = "排除标签不能为空")
    @Size(max = 500, message = "排除标签长度不能超过500个字符")
    private String negativeTags;

    /**
     * 音乐风格
     */
    @NotBlank(message = "音乐风格不能为空")
    @Size(max = 1000, message = "音乐风格长度不能超过1000个字符")
    private String style;

    /**
     * 上传音频URL
     */
    @NotBlank(message = "上传URL不能为空")
    @URL(message = "上传URL格式不正确")
    private String uploadUrl;

    /**
     * 业务参数校验
     */
    public void validateBusinessRules() {
        validateWeightParameters();

        // 人声生成只支持特定模型 - 使用修正后的方法
        SunoModelEnum[] supportedModels = {SunoModelEnum.V4_5PLUS, SunoModelEnum.V5};
        validateModelSupport(supportedModels, "添加人声");
    }

    /**
     * 构建添加人声请求
     */
    public static SunoAddVocalsRequest of(String uploadUrl, String prompt, String title, String negativeTags,
                                          String style, SunoModelEnum model, String callBackUrl) {
        SunoAddVocalsRequest request = new SunoAddVocalsRequest();
        request.setUploadUrl(uploadUrl);
        request.setPrompt(prompt);
        request.setTitle(title);
        request.setNegativeTags(negativeTags);
        request.setStyle(style);
        request.setModel(model);
        request.setCallBackUrl(callBackUrl);
        return request;
    }
}