package com.simply.ai.server.manager.model.request;

import com.simply.ai.server.manager.enums.SunoModelEnum;
import lombok.Data;
import lombok.EqualsAndHashCode;
import org.hibernate.validator.constraints.URL;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.Size;
import java.io.Serializable;

/**
 * 添加伴奏生成音乐请求参数
 */
@Data
@EqualsAndHashCode(callSuper = true)
public class SunoAddInstrumentalRequest extends SunoBaseRequest implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 上传音频URL
     */
    @NotBlank(message = "上传URL不能为空")
    @URL(message = "上传URL格式不正确")
    private String uploadUrl;

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
     * 包含的音乐标签
     */
    @NotBlank(message = "音乐标签不能为空")
    @Size(max = 500, message = "音乐标签长度不能超过500个字符")
    private String tags;

    /**
     * 业务参数校验
     */
    public void validateBusinessRules() {
        validateWeightParameters();

        // 伴奏生成只支持特定模型 - 使用修正后的方法
        SunoModelEnum[] supportedModels = {SunoModelEnum.V4_5PLUS, SunoModelEnum.V5};
        validateModelSupport(supportedModels, "添加伴奏");
    }

    /**
     * 构建添加伴奏请求
     */
    public static SunoAddInstrumentalRequest of(String uploadUrl, String title, String negativeTags,
                                                String tags, SunoModelEnum model, String callBackUrl) {
        SunoAddInstrumentalRequest request = new SunoAddInstrumentalRequest();
        request.setUploadUrl(uploadUrl);
        request.setTitle(title);
        request.setNegativeTags(negativeTags);
        request.setTags(tags);
        request.setModel(model);
        request.setCallBackUrl(callBackUrl);
        return request;
    }
}